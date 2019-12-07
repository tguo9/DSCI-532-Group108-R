# Reflections: 2019-11-30

## App Strengths
- Comparison of neighbourhood crime counts easy to see in crime map with red = bad, green = good.
- User can staturate crime map color scale to rule out heavy outliers (downtown) and still see neighbourhood comparisons at neighbourhoods with lower crime rates.
- Crime trend chart easily allows user to dive deeper into a neighbourhood's crime changes across time or between sub time categories (hours, day of week, month).
- Dashboard layout is simple and doesn't overhwlem the user.

## App Weaknesses
- Crime map unit of measurement is normalized crime counts. The general public may not understand what normalization is and this could cause confusion. To help the user, a tool tip was added to the map with raw crime counts, which is a more familiar unit of measurement.
- Some neighbourhoods artificially look better on the crime map than actual because they are smaller. A better crime index than count to use would be to use count/person, count/household, or count/area for each neighbourhood.
- App does not allow crime trends to be compared between neighbourhoods.
- The crime inndex slider bar is slow to update the crime map (takes a few seconds).

## Future Improvements
- Adjust crime map metric to be weighted per size or population of neighbourhood. Neighbourhood size/population is not included in the native data set, so this would need to be obtained from an alternative source.
- Add second line to crime trend chart for the user to select a second neighbourhood for comparison.
- Polish the app with formatting by creating a webste .css file (darken sliderbar colors).

## TA Feedback
- Updated teamwork contract to include general roles and responsibilities.
- In dropdown, restricted user to select one neighbourhood for crime trend chart.
- README.md file updated to highlight functionality of crime trend chart for various time scales.
