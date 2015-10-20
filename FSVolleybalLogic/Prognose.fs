namespace FSVolleybalLogic

    type ValidFinalStand =
    | ``S4-0``
    | ``S3-1``
    //| ``S2-2`` is not an valid final stand
    | ``S3-2``
    | ``S2-3``
    | ``S1-3``
    | ``S0-4``
    | ``S-``

    type CalcualteStandChangesModel = Team -> Team -> (ValidFinalStand * float) list

    type MatrixPrognoseRow = {Team:string; Results: ValidFinalStand list}
    type MatrixPrognose = {Opponents: string list; Rows: MatrixPrognoseRow list}

    module Prognose =
        let calcualteStandChangesOnSets (team1:Team) (team2:Team) =
            let pTeam1 = team1.PWinSet / (team1.PWinSet + team2.PWinSet)
            let pTeam2 = team2.PWinSet / (team1.PWinSet + team2.PWinSet)
            let draw = 6.0*pTeam1**2.0*pTeam2**2.0
            [
                ``S4-0``, (pTeam1**4.0)
                ``S3-1``, (4.0*pTeam1**3.0*pTeam2)
                ``S3-2``, draw*pTeam1
                ``S2-3``, draw*pTeam2
                ``S1-3``, 4.0*pTeam1*pTeam2**3.0
                ``S0-4``, pTeam2**4.0
            ]

        let calcualteStandChangesOnPoints (team1:Team) (team2:Team) =
            let pTeam1 = team1.PWinPoint / (team1.PWinPoint + team2.PWinPoint)
            let pTeam2 = team2.PWinPoint / (team1.PWinPoint + team2.PWinPoint)
            let draw = 6.0*team1.PWinSet**2.0*team2.PWinSet**2.0
            [
                ``S4-0``, (pTeam1**4.0)
                ``S3-1``, (4.0*pTeam1**3.0*pTeam2)
                ``S3-2``, draw*pTeam1
                ``S2-3``, draw*pTeam2
                ``S1-3``, 4.0*pTeam1*pTeam2**3.0
                ``S0-4``, pTeam2**4.0
            ]

        let creatPrognoseMatrix (calcualteStandChanges:CalcualteStandChangesModel)  (teams: Team list) = 
            let rec _createRow team opponents = 
                match opponents with
                | [] -> []
                | head::tail ->
                    if head = team then
                        ``S-`` ::( _createRow team tail)
                    else
                        ((calcualteStandChanges team head)|>List.maxBy (fun (_,change) -> change ) |> fst) :: ( _createRow team tail)

            {
                Opponents = ""::(teams |> List.map (fun t -> t.Name));
                Rows = teams |> List.map (fun team -> {Team = team.Name; Results = _createRow team teams})
            }

        let createPrognoseMatrixOnSet (teams:Team list) =
            creatPrognoseMatrix calcualteStandChangesOnSets teams

        let createPrognoseMatrixOnPoints (teams:Team list) =
            creatPrognoseMatrix calcualteStandChangesOnPoints teams

        let ValidFinalStandToString (stand:ValidFinalStand) =
            sprintf "%A" stand
        
        let printPrognoseMatrixCSV (matrix: MatrixPrognose) =
            matrix.Opponents |> List.iter (printf "%s;")
            printf "\n"
            let printRow (row: MatrixPrognoseRow) =
                printf "%s;" row.Team
                row.Results |> List.iter (fun r -> printf "%A;" r)
                printf "\n"
            matrix.Rows |> List.iter printRow