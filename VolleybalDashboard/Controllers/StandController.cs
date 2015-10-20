using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using FSVolleybalLogic;

namespace VolleybalDashboard.Controllers
{
    public class StandController : Controller
    {
        public string CompetitionStandURL => "http://www.volleybal.nl/application/handlers/export.php?format=rss&type=poule&standen=2DH&iRegionId=9000";
        // GET: Stand
        public ActionResult Index()
        {
            var xml = CompetitionStand.getCompetitionStandXML(CompetitionStandURL);
            return View(CompetitionStand.getTeams(xml));
        }

        public ActionResult PrognoseMatrixOnSets()
        {
            var xml = CompetitionStand.getCompetitionStandXML(CompetitionStandURL);
            var teams = CompetitionStand.getTeams(xml);
            var matrix = Prognose.createPrognoseMatrixOnSet(teams);
            return View("PrognoseMatrix", matrix);
        }

        public ActionResult PrognoseMatrixOnPoints()
        {
            var xml = CompetitionStand.getCompetitionStandXML(CompetitionStandURL);
            var teams = CompetitionStand.getTeams(xml);
            var matrix = Prognose.createPrognoseMatrixOnPoints(teams);
            return View("PrognoseMatrix", matrix);
        }
    }
}