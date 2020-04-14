#' Converts tracking data from wide to long
#'
#' Very tightly tied to how https://github.com/metrica-sports/sample-data is
#' organised right now since I don't know how tracking data is otherwise
#' organised
#'
#' @param dtTrackingSlice
#' @examples
#' @import data.table
#' @export
fConvertTrackingDataWideToLong = function(
    dtTrackingSlice
) {

    # all the intersects and all are happening because I had merged the 
    # tracking data with the event data earlier.
    # it's a bad idea to do that so going back to keeping them apart

    # this will be a very expensive function for a large number of rows so 
    # we could optimise it to be handled in RAM by breaking the operation
    # in chunks

    if ( T ) {

        # dtAllData = copy(dtTrackingSlice)

        dtTrackingSlice = melt(
            dtTrackingSlice[,
                c(
                    intersect(
                        c('Period','Frame','Time_s'),
                        colnames(dtTrackingSlice)
                    ),
                    grep(
                         colnames(dtTrackingSlice),
                         pattern = 'Ball|Player',
                         value = T
                    )
                ),
                with = F
            ],
            intersect(
                c('Period','Frame','Time_s'),
                colnames(dtTrackingSlice)
            )
        )

    } else {

     #     dtTrackingSlice = copy(dtTrackingSlice)

        dtTrackingSlice = melt(
            dtTrackingSlice,
            intersect(
                colnames(dtTrackingSlice),
                c('Period','Frame','Time_s','Team','Type','Subtype','From','To')
            )
        )

    }

    dtTrackingSlice = dtTrackingSlice[!is.na(value)]

    dtTrackingSlice[,
        Coordinate := gsub(
            x = variable,
            pattern = '.*Player[[:digit:]]*|.*Ball[[:digit:]]*',
            replacement = ''
        ),
        variable
    ]

    dtTrackingSlice[
         !Coordinate %in% c('X','Y'),
         Coordinate := NA
    ]

    dtTrackingSlice[,
        Tag := gsub(
            x = variable,
            pattern = 'Ball.*|Player.*',
            replacement = ''
        ),
        variable
    ]

    dtTrackingSlice[
        Tag %in% c(''),
        Tag := 'Ball'
    ]

    dtTrackingSlice[
         !Tag %in% c('Home','Away','Ball'),
         Tag := NA
    ]

    dtTrackingSlice[,
        Player := gsub(
            x = variable,
            pattern = paste0(paste0(unique(Coordinate), '$*'), collapse = '|'),
            replacement = ''
        ),
        variable
    ]

    # dtTrackingSlice[
    #       Player %in% c("EndFrame", "EndTime_s"),
    #       Player := NA
    # ]

    dtTrackingSlice[, variable := NULL]

    if ( F ) {

        dtTrackingSlice = merge(
            dtTrackingSlice[
                    1,
                    intersect(
                        colnames(dtTrackingSlice),
                        c(
                        'Period',
                        'Frame',
                        'Time_s',
                        'Team',
                        'Type',
                        'Subtype',
                        'From',
                        'To'
                        )
                    ),
                    with = F
            ],
            dcast(
                    dtTrackingSlice[
                        !is.na(Coordinate),
                        intersect(
                        colnames(dtTrackingSlice),
                        c(
                            'Period',
                            'Frame',
                            'Time_s',
                            'Tag',
                            'Player',
                            'Coordinate',
                            'value'
                        )
                        ),
                        with = F
                    ],
                    as.formula(
                        paste0(
                        paste0(
                            intersect(
                                    colnames(dtTrackingSlice),
                                    c('Period','Frame','Time_s','Tag','Player')
                            ),
                            collapse = '+'
                        ),
                        '~ Coordinate'
                        )
                    ),
                    value.var = 'value',
                    fun.aggregate = mean
            ),
            c(intersect(
                    colnames(dtTrackingSlice),
                    c('Period', 'Frame', 'Time_s')
            )),
            all = T
        )

    } else {

        dtTrackingSlice = dcast(
            dtTrackingSlice[
                    !is.na(Coordinate),
                    intersect(
                        colnames(dtTrackingSlice),
                        c(
                            'Period',
                            'Frame',
                            'Time_s',
                            'Tag',
                            'Player',
                            'Coordinate',
                            'value'
                        )
                    ),
                    with = F
            ],
            as.formula(
                    paste0(
                        paste0(
                            intersect(
                                    colnames(dtTrackingSlice),
                                    c('Period','Frame','Time_s','Tag','Player')
                            ),
                            collapse = '+'
                        ),
                        '~ Coordinate'
                    )
            ),
            value.var = 'value',
            fun.aggregate = mean
        )
        
    }

    dtTrackingSlice

}
