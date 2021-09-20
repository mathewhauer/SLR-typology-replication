# Organization
`000-Libraries.R` - sets up the work environment and loads the packages.

`001-Hammer-data-gather.R` - Gathers the data necessary to calculate the historic sub-county population estimates/projections. Requires the use of a CensusAPI key.

`002-Hammer-Calculation.R` - Does the raw historic and projected calculations. Requires the SSP county-level database which is available in the script and too large to store on GitHub.

`101-Figure1-DiceFig.r`, `102-Figure2-MultiMap.R`, `103-Figure3-Comparison.R`, and `alternative_figure4.R` reproduce the figures in the main document of the manuscript.


## License
The data collected and presented is licensed under the [Creative Commons Attribution 3.0 license](http://creativecommons.org/licenses/by/3.0/us/deed.en_US), and the underlying code used to format, analyze and display that content is licensed under the [MIT license](http://opensource.org/licenses/mit-license.php).