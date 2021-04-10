#' @export
fGetAttackingTeam = function(lData) {

   dtAttackingTeam = merge(
      rbind(
        lData$dtEventsData[
            Type %in% c(
                "PASS", "SHOT", "SET PIECE", "RECOVERY",
                "Catch","Catch save","Hold of ball",
                "Chance",
                "Cross", "Cross assist",
                "Pass", "Pass assist",
                "Reception",
                "Running with ball",
                "Shot on target", "Shot not on target"
            ) |
            Subtype %in% c("GOAL KICK", "KICK OFF"),
            list(AttackingTeam = Team[1]),
            list(Frame = StartFrame, EndFrame)
        ],
        # lData$dtEventsData[,
        #   list(
        #     StartFrame = StartFrame[
        #       grep(
        #         Type,
        #         pattern = "Out for"
        #       )
        #     ],
        #     EndFrame = EndFrame[
        #       grep(
        #         Type,
        #         pattern = "Out for"
        #       )
        #     ],
        #     Team = unlist(
        #       sapply(
        #         grep(
        #           Type,
        #           pattern = "Out for"
        #         ),
        #         function(x) {
        #           TeamTemp = Team[1:x]
        #           TeamTemp = TeamTemp[TeamTemp != '']
        #           setdiff(
        #             lData$dtEventsData[Team != '', unique(Team)],
        #             TeamTemp[length(TeamTemp)]
        #           )
        #         }
        #       )
        #     )
        #   )
        # ][,
        #   list(AttackingTeam = Team[1]),
        #   list(Frame = StartFrame, EndFrame)
        # ],
        data.table()
      ),
      data.table(Frame = lData$dtTrackingData[, unique(Frame)]),
      'Frame',
      all = T
   )

   dtAttackingTeam[is.na(EndFrame), EndFrame := Frame]

   dtAttackingTeam = dtAttackingTeam[,
      .SD[which.max(EndFrame)],
      list(Frame)
   ][
      order(Frame)
   ][,
      AttackingTeam := na.locf(AttackingTeam, na.rm = F)
   ]

   dtAttackingTeam[, EndFrame := NULL]

   dtAttackingTeam

}
