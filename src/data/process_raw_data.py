import pandas as pd
#import ujson
import json
import random
import csv

# set filenames
businessfile = '../../data/raw/yelp_academic_dataset_business.json'
reviewfile = '../../data/raw/yelp_academic_dataset_review.json'
userfile = '../../data/raw/yelp_academic_dataset_user.json'
checkinfile = '../../data/raw/yelp_academic_dataset_checkin.json'
tipfile = '../../data/raw/yelp_academic_dataset_tip.json'

# set filesname for csv
businesscsv = '../../data/interim/business.csv'
reviewcsv = '../../data/interim/review.csv'
usercsv = '../../data/interim/user.csv'
checkincsv = '../../data/interim/checkin.csv'
tipcsv = '../../data/interim/tip.csv'


with open(businessfile, 'r') as f:
    businessheader = [str(key) for key in json.loads(f.readline())]
    
with open(reviewfile, 'r') as f:
    reviewheader = [str(key) for key in json.loads(f.readline())]

with open(userfile, 'r') as f:
    userheader = [str(key) for key in json.loads(f.readline())]
                  
with open(checkinfile, 'r') as f:
    checkinheader = [str(key) for key in json.loads(f.readline())]

with open(tipfile, 'r') as f:
    tipheader = [str(key) for key in json.loads(f.readline())]


# Define function to write lines of a json to a csv file
# Function processes each line separately to prevent reading entire .json into memory
def json_to_csv(filename, header, csvfilename):
    print 'Converting {0} to {1}'.format(filename, csvfilename)
    count = 0
    step = 100000

    outputcsv = open(csvfilename, 'w')
    csvwriter = csv.writer(outputcsv)

    with open(filename, 'r') as f:
        csvwriter.writerow(header)
        for line in f:
            count += 1
            row = json.loads(line)
            csvwriter.writerow([unicode(v).encode("utf-8") for k,v in row.iteritems()])
            if (count % step) == 0:
                print 'Read {0} records'.format(count)
            
    print 'Finished output to {0}'.format(csvfilename)
    outputcsv.close()


# convert all the jsons, outputting to data/interim
json_to_csv(businessfile, businessheader, businesscsv)
json_to_csv(reviewfile, reviewheader, reviewcsv)
json_to_csv(userfile, userheader, usercsv)
json_to_csv(checkinfile, checkinheader, checkincsv)
json_to_csv(tipfile, tipheader, tipcsv)


# define function to sample csv and output another csv
def output_sample(intputfile, outputfile, sample_percent):
    n = sum(1 for line in open(intputfile)) - 1
    s = int(n*sample_percent)
    skip = sorted(random.sample(xrange(1,n+1),n-s))
    pd.read_csv(intputfile, skiprows=skip).to_csv(outputfile)
    print 'Completed output of sample to {0}'.format(outputfile)


output_sample(reviewcsv, '../../data/interim/review_sample.csv', .05)
output_sample(usercsv, '../../data/interim/user_sample.csv', .3)