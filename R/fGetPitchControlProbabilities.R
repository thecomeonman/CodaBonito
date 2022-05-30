#' Pitch control
#'
#' @param lData output of fParseTrackingDataBothTeams
#' @param viTrackingFrame frames you want processed
#' @param params has defaults in the function code but refer to the function
#' code to see what values are there that you can override
#' @param nYSpan pitch dimensions, sideline to sideline, centre circle is 0,0
#' @param nXSpan pitch dimensions goal to goal, centre circle is 0,0
#' @param iGridCellsX resolution of the calculation
#' @param bGetPlayerProbabilities Whether only the overall probability per team
#' should be returned or even the deatils of the break up between players
#' should be returned
#' @examples
#' @import data.table
#' @import zoo
#' @export
fGetPitchControlProbabilities = function (
    lData,
    viTrackingFrame,
    params = c(),
    nYSpan = 80,
    nXSpan = 120,
    iGridCellsX = 120,
    bGetPlayerProbabilities = F,
    bVerbose = F,
    vnXArray = NULL,
    vnYArray = NULL
) {

    ################################################################################
    # Frame details extraction
    ################################################################################
    {

        paramsFillin = c()

        if ( !'time_to_control_veto' %in% names(params) ) {
            params['time_to_control_veto'] = 3
        }

        if ( !'tti_sigma' %in% names(params) ) {
            params['tti_sigma'] = 0.45 # Standard deviation of sigmoid function in Spearman 2018 ('s') that determines uncertainty in player arrival time
        }


        if ( !'kappa_def' %in% names(params) ) {
            params['kappa_def'] = 1.72 # kappa parameter in Spearman 2018 (=1.72 in the paper) that gives the advantage defending players to control ball, I have set to 1 so that home & away players have same ball control probability
        }

        if ( !'lambda_att' %in% names(params) ) {
            params['lambda_att'] = 4.3 # ball control parameter for attacking team
        }

        if ( !'lambda_def' %in% names(params) ) {
            params['lambda_def'] = 4.3 * params['kappa_def'] # ball control parameter for defending team
        }

        # model parameters
        paramsFillin['max_player_accel'] = 7. # maximum player acceleration m/s/s, not used in this implementation
        paramsFillin['max_player_speed'] = 5. # maximum player speed m/s
        paramsFillin['reaction_time'] = 0.7 # seconds, time taken for player to react and change trajectory. Roughly determined as vmax/amax
        paramsFillin['average_ball_speed'] = 15. # average ball travel speed in m/s
        # numerical parameters for model evaluation
        paramsFillin['int_dt'] = 0.04 # integration timestep (dt)
        paramsFillin['max_int_time'] = 10 # upper limit on integral time
        paramsFillin['model_converge_tol'] = 0.01 # assume convergence when PPCF>0.99 at a given location.
        # The following are 'short-cut' parameters. We do not need to calculated PPCF explicitly when a player has a sufficient head start.
        # A sufficient head start is when the a player arrives at the target location at least 'time_to_control' seconds before the next player
        # params['time_to_control_att'] = params['time_to_control_veto']*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_att'])
        # params['time_to_control_def'] = params['time_to_control_veto']*np.log(10) * (np.sqrt(3)*params['tti_sigma']/np.pi + 1/params['lambda_def'])
        paramsFillin['time_to_control_att'] = params['time_to_control_veto']*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_att'])
        paramsFillin['time_to_control_def'] = params['time_to_control_veto']*log(10) * (sqrt(3)*params['tti_sigma']/pi + 1/params['lambda_def'])

        params = c(
            params,
            paramsFillin[!names(paramsFillin) %in% names(params)]
        )

        if ( bGetPlayerProbabilities ) {

            params['time_to_control_att'] = Inf
            params['time_to_control_def'] = Inf

        }

        rm(paramsFillin)

    }

    if ( bVerbose ) {
        print('Params done')
    }


    ################################################################################
    # Frame details extraction
    ################################################################################
    {

        # iTrackingFrame = lData$dtEventsData[Type == 'PASS', sample(StartFrame, 1)]
        # iTrackingFrame = lData$dtEventsData[821, StartFrame]
        dtTrackingSlice = lData$dtTrackingData[
            Frame %in% viTrackingFrame
        ]

        # ugly backward compatibility
        #
        if ( 'AttackingTeam' %in% colnames(dtTrackingSlice) ) {
            dtTrackingSlice[, AttackingTeam := NULL]
        }

        dtTrackingSlice[,
            intersect(
                colnames(dtTrackingSlice),
                c('Period','Time_s')
            ) := NULL
        ]

        if ( 'AttackingTeam' %in% colnames(lData$dtTrackingData) ) {

            dtAttackingTeam = lData$dtTrackingData[, list(AttackingTeam = AttackingTeam[1]), Frame]

        } else {

            # last team to make a pass is in control
            dtAttackingTeam = fGetAttackingTeam(lData)

            dtAttackingTeam = dtAttackingTeam[
                Frame %in% viTrackingFrame
            ]

            viTrackingFrame2 = intersect(
                dtAttackingTeam[, Frame],
                viTrackingFrame
            )

            if ( length(viTrackingFrame2) < length(viTrackingFrame)) {

                warning(
                    paste(
                        "Dropping frames - ",
                        paste(
                            setdiff(
                                viTrackingFrame,
                                viTrackingFrame2
                            ),
                            collapse = ', '
                        ),
                        " since the ball is probably not in possession of any team at these points"
                    )
                )

            }

        }


        if ( bVerbose ) {
            print('Attacking team done')
        }

    }



    ################################################################################
    # Pitch control probability calculation
    ################################################################################
    {

        if ( all(is.null(vnXArray)) ) {

            dx = nXSpan/iGridCellsX
            dy = nYSpan / round(nYSpan / ( nXSpan/iGridCellsX ) )

            vnXArray = seq(
                (-nXSpan/2) + ( 0.5 * dx ),
                (+nXSpan/2) - ( 0.5 * dx ),
                dx
            )
            vnYArray = seq(
                (-nYSpan/2) + ( 0.5 * dy ),
                (+nYSpan/2) - ( 0.5 * dy ),
                dy
            )

        }

        # vnXArray = seq(-nXSpan/2, nXSpan/2, nXSpan/iGridCellsX)
        # vnYArray = seq(-nYSpan/2, nYsSpan/2, nYSpan / round(nYSpan / ( nXSpan/iGridCellsX )))
        # print(vnXArray)
        # print(vnYArray)
        # stop()


        dtDetails = data.table(
            expand.grid(
                Frame = viTrackingFrame,
                TargetX = vnXArray,
                TargetY = vnYArray
            )
        )

        dtDetails[, SNO := .I]

        dtDetails = merge(
            dtDetails,
            dtAttackingTeam,
            'Frame'
        )

        dtDetails = merge(
            dtDetails,
            dtTrackingSlice[
                Tag == 'B'
            ][
                Frame %in% viTrackingFrame
            ][,
                list(
                    Frame,
                    BallX = X,
                    BallY = Y
                )
            ],
            'Frame'
        )

        # ball travel time is distance to target position from current ball position
        # divided assumed average ball speed
        #
        # difference from laurie's code
        # laurie's code used the pass start x,y coordinates but i use the ball's
        # tracked x,y coordinates instead
        dtDetails[,
            ball_travel_time := sqrt(
                ( ( TargetX - BallX ) ^ 2 ) +
                ( ( TargetY - BallY ) ^ 2 )
            ) / params['average_ball_speed']
        ]

        dtDetails[, c('BallX','BallY') := NULL]

        dtTrackingSliceVectorised = dtTrackingSlice[,
            list(SNO = unlist(dtDetails[, SNO])),
            by = c(colnames(dtTrackingSlice))
        ]

        dtTrackingSliceVectorised = merge(
            dtTrackingSliceVectorised,
            dtDetails,
            c('SNO','Frame')
        )

        # first get arrival time of 'nearest' attacking player (nearest also dependent on current velocity)
        dtTrackingSliceVectorised[
            Tag != 'B',
            time_to_intercept := fSimpleTimeToIntercept(
                reaction_time = pmax(
                    -Inf,
                    params['reaction_time']
                    #  - (
                    #     Time_s - dtAttackingTeam[,
                    #         StartTime_s
                    #     ]
                    # )
                ),
                VelocityX = VelocityX,
                VelocityY = VelocityY,
                position_x = X,
                position_y = Y,
                vmax = params['max_player_speed'],
                target_x = TargetX,
                target_y = TargetY
            )
        ]

        dtTrackingSliceVectorised[, c('X','Y') := NULL]
        dtTrackingSliceVectorised[, c('TargetX','TargetY') := NULL]
        dtTrackingSliceVectorised[, c('VelocityX','VelocityY') := NULL]


        vcUniqueTags = dtTrackingSliceVectorised[Tag != 'B', unique(Tag)]

        dtDetails = merge(
            dtDetails,
            setnames(
                dcast(
                    dtTrackingSliceVectorised[
                        Tag != 'B',
                        list(
                            tau_min = min(time_to_intercept)
                        ),
                        list(SNO, Tag)
                    ],
                    SNO ~ Tag,
                    value.var= 'tau_min'
                ),
                vcUniqueTags,
                c('tau_min_att','tau_min_def')
            ),
            c('SNO')
        )

        dtDetails[
            AttackingTeam == vcUniqueTags[2],
            c('tau_min_att','tau_min_def') := list(tau_min_def, tau_min_att)
        ]

        dtDetails[
            tau_min_att - pmax(ball_travel_time, tau_min_def) >= params['time_to_control_def'],
            AttackProbability := 0
        ]

        dtDetails[
            tau_min_def - pmax(ball_travel_time, tau_min_att) >= params['time_to_control_att'],
            AttackProbability := 1
        ]

        viSNOToEvaluate = dtDetails[(is.na(AttackProbability)), SNO]
        viSNOToEvaluate = sort(viSNOToEvaluate)

        setkey(
            dtTrackingSliceVectorised,
            SNO
        )

        # remove the ones which are already done
        dtTrackingSliceVectorised = merge(
            dtTrackingSliceVectorised,
            dtDetails[
                is.na(AttackProbability),
                list(SNO, tau_min_att, tau_min_def)
            ],
            'SNO'
        )

        dtDetails[, c('tau_min_def','tau_min_att') := NULL]

        dtTrackingSliceVectorised = dtTrackingSliceVectorised[(
                Tag != AttackingTeam &
                time_to_intercept - tau_min_def < params['time_to_control_def']
            ) | (
                Tag == AttackingTeam &
                time_to_intercept - tau_min_att < params['time_to_control_att']
            )
        ]

        dtTrackingSliceVectorised[, c('tau_min_def','tau_min_att') := NULL]

        dT_array = seq(
            -params['int_dt'],
            params['max_int_time'] - params['int_dt'],
            params['int_dt']
        )

        dtPPCF = data.table(
            AttackProbability = 0,
            DefenseProbability = 0
        )[,
            list(
                SNO = viSNOToEvaluate
            ),
            list(
                DefenseProbability,
                AttackProbability
            )
        ]

        vbSNOToEvaluationFlag = rep(T, length(viSNOToEvaluate))
        dtTrackingSliceVectorised[, PlayerPPCF := 0]

        i = 2

        dtProbabilities = data.table()

        repeat {

            # calculate ball control probablity for 'player' in time interval T+dt
            dtTrackingSliceVectorised = merge(
                dtTrackingSliceVectorised,
                dtPPCF,
                c('SNO'),
                all.x = T
            )

            dtTrackingSliceVectorised[
                !is.na(AttackProbability),
                PlayerPPCF :=
                    PlayerPPCF +
                    pmax(
                        (
                            1 -
                            AttackProbability -
                            DefenseProbability
                        ) *
                        fBallInterceptionProbability(
                            params['tti_sigma'],
                            time_to_intercept,
                            dT_array[i] + ball_travel_time
                        ) *
                        params[
                            paste0(
                                'lambda_',
                                ifelse(
                                    Tag == AttackingTeam,
                                    'att',
                                    'def'
                                )
                            )
                        ] *
                        params['int_dt'],
                        0
                    )
            ]

            dtPPCF = dtTrackingSliceVectorised[
                !is.na(AttackProbability),
                list(
                    DefenseProbability = sum(
                        PlayerPPCF[Tag != AttackingTeam]
                    ),
                    AttackProbability = sum(
                        PlayerPPCF[Tag == AttackingTeam]
                    )
                ),
                SNO
            ]

            dtProbabilities = rbind(
                dtProbabilities,
                dtPPCF[
                    AttackProbability + DefenseProbability > 1 - params['model_converge_tol'],
                    list(
                        SNO,
                        AttackProbability,
                        DefenseProbability
                    )
                ],
                fill = T
            )

            if ( dtProbabilities[, any(AttackProbability + DefenseProbability > 1 + params['model_converge_tol'])] ) {
                stop('Probabilities > 1. Look at dtProbabilities to debug.')
            }


            dtPPCF = dtPPCF[
                !SNO %in% dtProbabilities[, SNO]
            ]

            i = i + 1

            if ( i > length(dT_array) ) {
                break
            }

            if ( nrow(dtPPCF) == 0 ) {
                break
            }

            dtTrackingSliceVectorised = dtTrackingSliceVectorised[,
                c('AttackProbability','DefenseProbability') := NULL
            ]

        }


        if ( i > length(dT_array) ) {

            stop(
                paste0(
                    "Integration failed to converge for some cases"
                )
            )

            # print(
            #     paste0(
            #         'SNOS:',
            #         dtPPCF[, SNO]
            #     )
            # )

            # stop()

        }

        dtDetails = rbind(
            dtDetails[!is.na(AttackProbability)],
            merge(
                dtDetails[
                    is.na(AttackProbability)
                ][,
                    !'AttackProbability'
                ],
                dtProbabilities,
                'SNO',
                all = T
            ),
            fill = T
        )

        dtDetails[,
            DefenseProbability := NULL
        ]

        dtDetails[,
            ball_travel_time := NULL
        ]

    }

    dtDetails[, AttackingTeam := NULL]


    if ( !'AttackingTeam' %in% colnames(dtTrackingSlice) ) {
        dtTrackingSlice = merge(
            dtTrackingSlice,
            dtAttackingTeam,
            'Frame'
        )
    }

    lReturn = list(
        dtTrackingSlice = dtTrackingSlice,
        dtDetails = dtDetails
    )

    if ( bGetPlayerProbabilities ) {

        lReturn$dtTrackingSliceVectorised = dtTrackingSliceVectorised[,
            list(
                SNO, Player, PlayerPPCF
            )
        ]

    }

    lReturn

}
