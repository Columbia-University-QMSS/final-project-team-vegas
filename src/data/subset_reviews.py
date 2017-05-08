import pandas as pd
import csv

# reader for reviews
review_header = pd.read_csv('../../data/interim/review.csv', nrows = 0)
#review_rdr = pd.read_csv('../../data/interim/review.csv', chunksize = 10 ** 5 * 2)

# read in restaurants
print('Read in restaurants file')
#restaurants = pd.read_csv('../../data/processed/restaurants.csv', usecols = ['business_id'])
global_restaurants = pd.read_csv('../../data/processed/global_restaurants.csv', usecols = ['business_id'])
us_restaurants = pd.read_csv('../../data/processed/us_restaurants.csv', usecols = ['business_id'])

# define function which merges the reviews with a given subset file (restaurants, global_restaurants, us_restaurants) 
# and outputs the reviews to a csvfile

def subset_reviews(csvfilename, business_subset, sample = 1.0):
	print('Write header for {0}').format(csvfilename)
	#csvfile = '../../data/processed/{0}'.format(csvfilename)
	outputcsv = open(csvfilename, 'w')
	csvwriter = csv.writer(outputcsv)
	csvwriter.writerow(list(review_header))
	outputcsv.close()
	print('Merge subset with reviews, with {0}% sample and output {1}').format(sample*100, csvfilename)
	count = 0
	review_rdr = pd.read_csv('../../data/interim/review.csv', chunksize = 10 ** 5 * 2)
	with open(csvfilename, 'a') as f:
		for lines in review_rdr:
			data = pd.merge(lines, business_subset, on = 'business_id', how = 'inner')
			data.sample(frac = sample).to_csv(f, header = False)
			count += data.size
			print('Processed {0} reviews'.format(count))
	print('Finished output of {0}').format(csvfilename)


# subset global restaurant reviews
subset_reviews('../../data/processed/gbl_restaurant_reviews_50.csv', global_restaurants, .50)
subset_reviews('../../data/processed/gbl_restaurant_reviews_10.csv', global_restaurants, .10)

# subset us restaurant reviews
subset_reviews('../../data/processed/us_restaurant_reviews_50.csv', us_restaurants, .50)
subset_reviews('../../data/processed/us_restaurant_reviews_10.csv', us_restaurants, .10)