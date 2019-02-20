# Amara Holder

holder.amara@epa.gov

* AQY\ AB-282\ Data\ Export.csv
* 190129.txt

Hi Jon,
 
Sorry it has taken me so long to respond I was out of the office for the long weekend and digging through a tremendous backlog of emails.
 
It’s great to hear that you are looking for some sensor data ‘out in the wild’ because we do have some to share. We have a few different sensor packages that we have been sending out with the ARAs, so there several different formats and they likely will change as we work out some of the issues with lack of cellular coverage we have encountered near some of the fires.
 
I’ve attached a sample file from the Aeroqual AQY units. The column identifiers are included in the file along with a header identifying the source of the data. This file was manually downloaded from their cloud service, but they have an ftp or email option for automatic download. Let me know what transfer method would work best for you and I will work on getting  a sample data file.
 
I’ve also attached a sample file from our RAMP unit. Right now this system does not transmit data, but we are working on changing that for the next fire season. As soon as I get that sorted out I will send you a sample file. For now the data file has no headers, but includes a serial number, date/time stamp, and columns with the parameter identifier followed by the measurement. Everything after the RAW column are diagnostic values.
 
Let me know if you need additional information for now. I will keep you updated as we make changes to our systems. It would be awesome to be able to include some of our sensor data on PWFSLSmoke package for the next fire season.
 
Thanks,
Amara

# Sara Strachan

Sara.Strachan@deq.idaho.gov

* HrConc.CSV
* E-Sequential\ Example\ File.csv

Hi Jon,
 
I’ve got one not-very-challenging file for you (HrConc.csv). It is from our Met One PM2.5 BAM monitor. The other file (E-Sequential Example File.csv) is from a Met One PM2.5 E-Sequential sampler. It doesn’t contain a concentration column, but I’m throwing it in in case it might be useful to you. Here’s what our monitor data manager says about it:
 
“The file just contains the run data for the filter.  We don’t get the concentration till we pair it with the lab file containing the weight differences.  The weight difference is then divided by the sample volume for the resulting concentration.”
 
I hope this is helpful to you. Looking forward to the new site updates!
Cheers,


# Kris Ray

Kris.Ray@colvilletribes.com

* BAM2_data_190219_100627.csv

The file I just sent was downloaded from the BAM 1020 located in Nespelem WA.  I used the Met One Comet program to download the data. 

# Jill Schulte


jils461@ecy.wa.gov

* clarity-measurements-2019-02-19T15_00_09.519Z.cs

Good morning Jonathan,
 
Yes, this is an impressive challenge to undertake. I can provide one data file for you to test that is not available in AirNow Tech. We have been testing several Clarity Node-S units (https://clarity.io/solution). Data is uploaded to Clarity’s cloud server via cell modem on each unit. I’ve attached an example file of the output from the Clarity server. Please note that we have 4 different units - the “nodeId” column differentiates them.
 
We are generally able to manage this kind of data in-house with our own R scripts, but I hope this file can help you with your testing.
 
Let me know if you have any questions.

