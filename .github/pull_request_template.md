### Thank you for your Pull Request! 

We have developed a Pull Request template to aid you and our reviewers.  Completing the below tasks helps to ensure our reviewers can maximize their time on your code as well as making sure the adrgOS codebase remains robust and consistent.  

### Checklist

Please check off each taskbox as an acknowledgment that you completed the task. This checklist is part of the Github Action workflows and the Pull Request will not be merged into the `dev` branch until you have checked off each task.

- [ ] Code is formatted according to the [tidyverse style guide](https://style.tidyverse.org/) 
- [ ] Updated relevant unit tests or have written new unit tests.
- [ ] Creation/updates to relevant roxygen headers and examples.
- [ ] Run `devtools::document()` so all `.Rd` files in the `man` folder and the `NAMESPACE` file in the project root are updated appropriately
- [ ] Run `pkgdown::build_site()` and check that all affected examples are displayed correctly and that all new functions occur on the "Reference" page.
- [ ] Update NEWS.md if the changes pertain to a user-facing function (i.e. it has an @export tag) or documentation aimed at users (rather than developers)
- [ ] Address any updates needed for vignettes and/or templates
- [ ] Run `R CMD check` locally and address all errors and warnings - `devtools::check()`
- [ ] Link the issue so that it closes after successful merging. 
- [ ] Address all merge conflicts and resolve appropriately 
- [ ] Pat yourself on the back for a job well done!  Much love to your accomplishment!
