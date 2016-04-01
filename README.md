# foodmap
Visualization of food using a [self-organizing map](https://en.wikipedia.org/wiki/Self-organizing_map),
based on their nutrient value distances. Food data comes from [USDA National Nutrient Database](https://ndb.nal.usda.gov/).

# Usage
```
$ sbt run
```
This opens a window with the map on the left. Hover over a square to display the food associated with this position.

![screenshot](https://raw.githubusercontent.com/fdietze/foodmap/master/screenshot.png)

Color legend:
* **blue:** milk and milk products
* **red:** meat, poultry, fish, and mixtu
* **yellow:** eggs
* **brown:** legumes, nuts, and seeds
* **orange:** grain products
* **pink:** fruits
* **green:** vegetables
* **teal:** fats, oils, and salad dressing
* **purple:** sugars, sweets, and beverages
