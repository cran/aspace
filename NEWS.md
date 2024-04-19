# apsace 4.1.2
* Bugfixes
    * Found and fixed a few more little plotting bugs (thanks to John H. Lowry).
    * Made a few things more efficient.

# apsace 4.1.1
* Bugfixes
    * Fixed a minor loop indexing error that threw an error if all centre types were not selected.
    
# apsace 4.1.0
* Improvements
    * Inclusion of repaired and renamed function calc_cmd() that was previously CMD(). The change was to bring consistency across function naming.
    * Inclusion of repaired function plot_centres(). 
    * Renaming CF() to calc_cf() to bring consistency across function naming.
    * Renaming CF2PTS() to calc_cf2pts() to bring consistency across function naming.
    * Renaming mean_centre() to calc_mnc() to bring consistency across function naming.
    * Renaming median_centre() to calc_mdc() to bring consistency across function naming.
* Bugfixes
    * Major clean-up and refresh of man pages for most functions.
        
# aspace 4.0.1
* Improvements
    * Shortened title as required.
    * Updated package description. 
    * Added necessary Rd-tags on man pages.
    * Moved cat() statements to warning() and message() formats.
    * Ensured that user environments are properly restored after functions exit.

# aspace 4.0.0
* Major Items
    * Resurrecting aspace from its current archived state by addressing flags and errors.
    * Functions no longer write .txt files but rather return list objects.
    * Global variables are eliminated to adhere to R guidelines.
    * Calculation output objects are consistently encoded.
    * Functions provide greater error-trapping to ensure valid inputs are provided.
    * Functions CMD() and plot_centres() are currently disabled due to issues. Hope to have them active again for the next version.
    * Eliminated reliance on 'shapefiles' package.
* Improvements
    * Functions return consistently structured objects rather than writing them to the workspace.
* Bugfixes
    * Updates and adjustments to manual pages.
* Future Plans
    * Incorporate vector geographic data objects with package 'terra'.
    * Produce a generic plotting function, eliminating the need for multiple forms.

