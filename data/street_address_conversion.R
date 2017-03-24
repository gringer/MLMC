#!/usr/bin/Rscript

data.df <- read.csv("nz-street-address.csv.gz", stringsAsFactors=FALSE,
                    nrows = 2*10^6);

sub.df <- data.df[,c("full_road_name", "address_number",
                     "full_address", "gd2000_xcoord", "gd2000_ycoord")];
sub.df$addressDetail <-
    paste(sub.df$full_address,
          sub.df$gd2000_xcoord, sub.df$gd2000_ycoord, sep=";");
sub.df$roadIndex <- toupper(substr(gsub(" ","",sub.df$full_road_name),1,5));
sub.df <- sub.df[order(sub.df$roadIndex,
                       sub.df$full_road_name, sub.df$address_number,
                       sub.df$full_address),];

write.table(sub.df[,c("roadIndex", "address_number", "address_number", "addressDetail")],
            sep="\t", file="nz-street-address-NLonly.csv",
            row.names=FALSE, col.names=FALSE, quote=FALSE);
