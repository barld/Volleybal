
#load "Scripts/load-project.fsx"
#load "Scripts/load-references.fsx"
open FSharp.Data

type CompetitionStandXml = XmlProvider<"http://www.volleybal.nl/application/handlers/export.php?format=rss&type=poule&standen=2DH&iRegionId=9000">

let stand = CompetitionStandXml.GetSample()

type Team = {Name:string;Number:int;SetsWin:int;SetsLose:int;PWinSet:float}

let mapRankingToTeam (ranking:CompetitionStandXml.Ranking) =
    {Name = ranking.Team.Value; 
    Number = ranking.Nummer; 
    SetsWin=ranking.Setsvoor;
    SetsLose=ranking.Setstegen;
    PWinSet= (ranking.Setsvoor |> float) / ((ranking.Setsvoor + ranking.Setstegen) |> float)}

let teams =
    stand.Channel.Rankings
    |> Array.map mapRankingToTeam
    |> Array.toList

teams |> List.iter(fun t -> printfn "%A" t)

type FinalStand =
    | ``S4-0``
    | ``S3-1``
    //| ``S2-2`` is not an valid final stand
    | ``S3-2``
    | ``S2-3``
    | ``S1-3``
    | ``S0-4``
    | ``S-``

let CalcStandChanges (team1:Team) (team2:Team) =
    let draw = 6.0*team1.PWinSet**2.0*team2.PWinSet**2.0
    [
        ``S4-0``, (team1.PWinSet**4.0)
        ``S3-1``, (4.0*team1.PWinSet**3.0*team2.PWinSet)
        ``S3-2``, draw*team1.PWinSet
        ``S2-3``, draw*team2.PWinSet
        ``S1-3``, 4.0*team1.PWinSet*team2.PWinSet**3.0
        ``S0-4``, team2.PWinSet**4.0
    ]

CalcStandChanges teams.[0] teams.[1] |>List.maxBy (fun (_,change) -> change )

type MatrixPrognoseRow = {Team:string; Results: (FinalStand Option) list}
type MatrixPrognose = {Opponents: string list; Rows: MatrixPrognoseRow list}

let creatPrognoseMatrix (teams: Team list) = 
    let rec _createRow team opponents = 
        match opponents with
        | [] -> []
        | head::tail ->
            if head = team then
                None ::( _createRow team tail)
            else
                (Some ((CalcStandChanges team head)|>List.maxBy (fun (_,change) -> change ) |> fst)) :: ( _createRow team tail)

    {
        Opponents = ""::(teams |> List.map (fun t -> t.Name));
        Rows = teams |> List.map (fun team -> {Team = team.Name; Results = _createRow team teams})
    }
        
let printPrognoseMatrixCSV (matrix: MatrixPrognose) =
    matrix.Opponents |> List.iter (printf "%s;")
    printf "\n"
    let printRow (row: MatrixPrognoseRow) =
        printf "%s;" row.Team
        row.Results |> List.iter (fun r -> printf "%A;" (if r.IsSome then r.Value else ``S-``))
        printf "\n"
    matrix.Rows |> List.iter printRow

printPrognoseMatrixCSV (creatPrognoseMatrix teams)