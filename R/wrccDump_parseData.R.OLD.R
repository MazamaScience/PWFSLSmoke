# #' @keywords WRCC
# #' @export
# #' @title Read and Parse Single Monitor Data from WRCC
# #' @param file either a path to a WRCC data file the contents of the file as a character string
# #' @param verbose logical flag to generate verbose output
# #' @description A WRCC monitor data file is read in and parsed.
# #' @return Dataframe of WRCC monitor data.
# 
# wrcc_readData <- function(file, verbose=FALSE) {
#   
#   # Data can be downloaded from the DRI website:  http://www.wrcc.dri.edu/cgi-bin/smoke.pl
#   # The only output type that seems to be working for this site 'HTML' which looks like this:
#   
#   #   <!DOCTYPE html
#   #   PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
#   #   "http://www.w3.org/TR/html4/loose.dtd">
#   #   <html lang="en-US"><head><title>Station Data Listing - Smoke USFS R1-306</title>
#   #   <script type="text/javascript">
#   #   <!-- Hide script
#   #   // End script hiding -->
#   #   </script>
#   #   </head>
#   #   <body>
#   #   <BODY BGCOLOR="FFFFFF">
#   #   <CENTER>
#   #   <H1> Smoke USFS R1-306 </H1>
#   #   <PRE>
#   #   :         LST    Deg     Deg            ser #   ug/m3    Unk     l/m    Deg C     %      Unk    deg C     %      m/s     Deg    volts        
#   #   :   Date/Time   GPS     GPS   Type    Serial  Conc     Misc    Ave.    Av Air   Rel    Misc   Sensor  Sensor    Wind   Wind   Battery Alarm  
#   #   :YYYYMMDDhhmm   Lat.    Lon.          Number   RT       #1    Air Flw   Temp  Humidty   #2    Int AT  Int RH    Speed  Direc  Voltage        
#   #   201411062100 46.9271 -114.09       9 -169399       0  -998.9       0    18.1      44   90210  -998.9      40     0.3     205      14       0
#   #   201411062200 46.9271 -114.09       9 -169399       3  -998.9     1.7    19.9      39   90151  -998.9      34     1.2     283      14       0
#   #   ...
#   #   201511172200 45.4118 -116.32       9 -169399       1  -998.9       2     7.6      64   94957  -998.9      48     2.9      14    14.4       0
#   #   201511172300 45.4118 -116.32       9 -169399       1  -998.9       2     7.5      66   94840  -998.9      48       3       5    14.4       0
#   #   </PRE></CENTER>
#   
#   # Read in file and determine which part is the data:
#   lines <- readr::read_lines(file)
#   firstLine <- which(stringr::str_detect(lines,'^<PRE>')) + 1
#   lastLine <- which(stringr::str_detect(lines,'^</PRE>')) - 1
#   
#   # Extract the monitor name
#   nameLine <- which(stringr::str_detect(lines,'<H1>'))
#   monitorName <- stringr::str_sub(lines[nameLine],6,-7)
#   
#   if (verbose) cat(paste0('Processing data for "',monitorName,'"'))
#     
#   if (firstLine == lastLine) stop(paste0('No data found.'))
#   
#   
#   # So far, two kinds of headers have been seen. Most files have the following:
#   #
#   # :         LST    Deg     Deg            ser #   ug/m3    Unk     l/m    Deg C     %     mbar    deg C     %      m/s     Deg    volts        
#   # :   Date/Time   GPS     GPS   Type    Serial  Conc     Misc    Ave.    Av Air   Rel    Barom  Sensor  Sensor    Wind   Wind   Battery Alarm  
#   # :YYYYMMDDhhmm   Lat.    Lon.          Number   RT       #1    Air Flw   Temp  Humidty  Press  Int AT  Int RH    Speed  Direc  Voltage        
#   #
#   # A second type is seen with smn1, smn2 and smn3:
#   #
#   # :         LST    Unk    ug/m3    l/m     m/s     Deg    Deg C     %       %     volts    Deg     Deg 
#   # :   Date/Time  Misc   Conc     Ave.     Wind   Wind    Av Air   Rel   Sensor  Battery   GPS     GPS  
#   # :YYYYMMDDhhmm   #1     RT     Air Flw   Speed  Direc    Temp  Humidty Int RH  Voltage   Lat.    Lon. 
#   #
#   # Note from Readme.rtf that came with the archived data:
#   #
#   #   All times are in GMT
#   #   Unknown Misc #1 head for files starting with e (ebam) indicates hrly value
#   
#   
#   # Prepare header lines
#   headerLines <- lines[firstLine:(firstLine+2)]
#   headerLines <- stringr::str_sub(headerLines,2,-1) # Strip off first ':' so they will align with the data
#   
#   # Determine col_positions based on the length of first header line.
#   lineLength <- stringr::str_length(headerLines[1])
#   
#   if ( lineLength == 140 ) {
# 
#     # The following ruler and sample data line help figure out the start and end positions:
#     #
#     #         10        20        30        40        50        60        70        80        90       100       110       120       130       140
#     # 00000000011111111112222222222333333333344444444445555555555666666666677777777778888888888999999999900000000001111111111222222222233333333334444444444
#     # 1            4       2       0       8       6       4       2       0       8       6       4       2       0       8       6       4 
#     # 201511172200 45.4118 -116.32       9 -169399       1  -998.9       2     7.6      64   94957  -998.9      48     2.9      14    14.4       0
#     
#     # Prepare positions for fixed width fields (fwf)
#     start <- c(1,14, 22, 30, 38, 46, 54, 62, 70, 78, 86, 94, 102, 110, 118, 126, 134)
#     end <- c(start[-1] - 2, lineLength)
#     col_positions <- readr::fwf_positions(start,end)
#     
#   } else if (lineLength == 100 ) {
#     
#     # The following ruler and sample data line help figure out the start and end positions:
#     #
#     #         10        20        30        40        50        60        70        80        90       100       
#     # 0000000001111111111222222222233333333334444444444555555555566666666667777777777888888888899999999990000000000
#     # 1            4       2       0       8       6       4       2       0       8       6       4
#     # 201406161700    -999    -999   -8189    -999    -999    -999    -999    -999    -999 35.3499 -77.669
# 
#     # Prepare positions for fixed width fields (fwf)
#     start <- c(1,14, 22, 30, 38, 46, 54, 62, 70, 78, 86, 94)
#     end <- c(start[-1] - 2, lineLength)
#     col_positions <- readr::fwf_positions(start,end)
#     
#   } else {
#     
#     cat(paste0(lines[firstLine:(firstLine+2)],collapse='\n'))
#     stop('File format not supported. First header line != 140 or 10 characters.')
#     
#   }
#     
#   # Parse header ---------------------
#   
#   # Read in the header
#   headerLines <- paste0(headerLines,collapse='\n')        # Create a single strings with newlines
#   headerDF <- readr::read_fwf(headerLines, col_positions) # Parse this as a fwf dataframe
#   
#   # Create R-friendly column units and names directly from the header data
#   characterMatrix <- as.matrix(headerDF)
#   colUnits <- characterMatrix[1,]
#   colNames <- make.names(paste0(characterMatrix[2,],' ',characterMatrix[3,]))
#   
#   # Parse data -----------------------
#   
#   # There can be additional header lines beginning with ':' embedded in the data. We pull these out here.
#   
#   dataLines <- lines[firstLine:lastLine]
#   headerMask <- stringr::str_detect(dataLines,'^:')
#   dataLines <- dataLines[!headerMask]
# 
#   dataLines <- paste0(dataLines,collapse='\n')            # Create a single strings with newlines
#   df <- readr::read_fwf(dataLines, col_positions)         # Parse this as a fwf dataframe
#   
#   # Assign column names
#   names(df) <- colNames
#   
#   # Add monitor name
#   df$monitorName <- monitorName
#   
#   # Add E-BAM Type if needed
#   if (lineLength == 100) df$Type == 0
#   
#   return(df)
#   
# }
