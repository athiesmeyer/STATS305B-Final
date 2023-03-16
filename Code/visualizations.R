source("Code/process_features.R")

library(ggplot2)

hosp.stays <- ggplot(data=data, aes(x=hosp.stays)) +
                geom_histogram(binwidth=1) +
                labs(x="Hospital Stays",
                     y="",
                     title="Distribution of Hospital Stays") +
                xlim(-1, 25)

er.visits <- ggplot(data=data, aes(x=er.visits)) +
                geom_histogram(binwidth=1) +
                labs(x="ER Visits",
                     y="",
                     title="Distribution of ER Visits") +
                xlim(-1, 25)

doctor.visits <- ggplot(data=data, aes(x=doctor.visits)) +
                   geom_histogram(binwidth=1) +
                   labs(x="Doctor Visits",
                        y="",
                        title="Distribution of Doctor Visits") +
                   xlim(-1, 50)
