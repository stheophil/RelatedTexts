import json
import sys

if len(sys.argv) < 1:
	print("Missing filename")

file = sys.argv[1]

with open(file, "r") as f:
	json = json.loads( f.read() )

	matches = 0
	rating = 0.0
	for obj in json:
		matches += 1
		rating += obj['rating']
		print("rating = %f" % obj['rating'])

	print("%d matches with avg rating of %f" % (matches, rating/matches))