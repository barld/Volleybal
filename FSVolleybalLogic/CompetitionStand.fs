namespace FSVolleybalLogic
    open FSharp.Data

    type CompetitionStandXml = XmlProvider<"http://www.volleybal.nl/application/handlers/export.php?format=rss&type=poule&standen=2DH&iRegionId=9000">

    type Team = 
        {
            Name:string;
            Number:int;
            Matches:int;
            SetPoints:int;
            SetsWin:int;
            SetsLose:int;
            PWinSet:float;
            PWinPoint:float
        }

    module CompetitionStand =
        let mapRankingToTeam (ranking:CompetitionStandXml.Ranking) =
            {Name = ranking.Team.Value; 
            Number = ranking.Nummer; 
            Matches = ranking.Wedstrijden;
            SetPoints = ranking.Punten;
            SetsWin=ranking.Setsvoor;
            SetsLose=ranking.Setstegen;
            PWinSet = (ranking.Setsvoor |> float) / ((ranking.Setsvoor + ranking.Setstegen) |> float);
            PWinPoint = (ranking.Punten |> float) / ((ranking.Wedstrijden*5) |> float)
            }

        let getCompetitionStandXML (uri : string) =
            CompetitionStandXml.Load(uri)

        let getTeams (competitionStand : CompetitionStandXml.Rss) =
            competitionStand.Channel.Rankings
            |> Array.map mapRankingToTeam
            |> Array.toList

        

