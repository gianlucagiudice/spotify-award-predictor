mongoimport -d spotify -c data --drop --type csv --file ../data/data_cleaned.csv --headerline
mongoimport -d spotify -c awards --drop --type csv --file ../data/awards_cleaned.csv --headerline
