# Reflections: 2019-11-30 (Updated 2019-12-07)

## App Strengths
- Comparison of neighbourhood crime counts easy to see in crime map with red = bad, green = good.
- User can staturate crime map color scale to rule out heavy outliers (downtown) and still see neighbourhood comparisons at neighbourhoods with lower crime rates.
- Crime trend chart easily allows user to dive deeper into a neighbourhood's crime changes across time or between sub time categories (hours, day of week, month). Two neighbourhoods can also be compared to see trends within each.
- Dashboard layout is simple and doesn't overhwlem the user.

## App Weaknesses
- Crime map unit of measurement is normalized crime counts. The general public may not understand what normalization is and this could cause confusion. To help the user, a tool tip was added to the map with raw crime counts, which is a more familiar unit of measurement, and an explaianation of crime index.
- Some neighbourhoods artificially look better on the crime map than actual because they are smaller. A better crime index than count to use would be to use count/person, count/household, or count/area for each neighbourhood.
- The crime inndex slider bar is slow to update the crime map (takes a few seconds).

## Future Improvements
- Adjust crime map metric to be weighted per size or population of neighbourhood. Neighbourhood size/population is not included in the native data set, so this would need to be obtained from an alternative source.
- Polish the app with formatting by creating a webste .css file (darken sliderbar colors).
- Make X-axis of crime trend chart easier to read than intergers.

## TA Feedback Milestone 1
- Updated teamwork contract to include general roles and responsibilities.
- In dropdown, restricted user to select one neighbourhood for crime trend chart.
- README.md file updated to highlight functionality of crime trend chart for various time scales.

## Peer Feedback Milestone 2
1. Explain Crime Index (Add definition with explanation of crime index)
2. Make time scale dropdown names not as crude
3. Add introduction to dashboard
4. Tooltip still shows ratio, whereas crime index on legend
5. Have two lines for multiple neighbourhoods in line chart
6. Speed issues of choropeth crime index (Please wait to load)
7. Time scale for month/week/hour (explain this is total counts in year)
8. Size issues if people aren't using 100% in there browser
9. Highlight individual neighbourhood on choropeth once selected
10. Add rank to choropeth tool tip
11. Add tooltip to line chart

## Updates from Peer Feedback Milestone 2:
- Peer feedback points 1-5 were implemented. Point 1 was prioritized because it was the most common response from all three review sessions. Points 2-4 were implemented because they were quick upgrades. Point 5 was deemed the most helpful complex upgrade to add compared to 10.
- Points 6 and 8 were deemed too complex to manage in the time frame provided for updates.
- Points 7, 9, and 11 were deemed unecessary 'nice-to-haves'.

## TA Feedback Milestone 2
A. Add App description at beginning
B. Change color scale (GDP?)
C. Cite data source

## Updates from TA Feedback Milestone 2
- Point A was included via peer feedback
- Point B was unclear and not included (GDP) is not in dashboard.
- Point C will be implemented in future updates as time did not permit incorporation