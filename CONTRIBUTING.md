# Contributing to ghg-cprg

This project uses a [feature-branch](https://deepsource.io/blog/git-branch-naming-conventions/) naming convention and workflow.


## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file, such as `.md`, `.Rmd` or `.R`. 

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 

If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

## Pull request process

`main` is the main branch. Base your work off of `main`.
Contribute to the project by making changes to your own feature branch and create a pull request when you're ready to integrate into the `main` branch. Here is our suggested workflow.

* Pull the `main` branch; `git pull`, and if necessary `git checkout main` to switch to `main`
* Create a feature branch and check out your branch, e.g., `git checkout -b bug-fix-1`
  * You can use your initials to prefix a feature branch, e.g., `aa-bug-fix-1`.
  * Your feature branch should do one thing only, for example: 
    * create a new function,
    * create a new custom variable,  
    * fix an issue - [please name your branch with the issue number](https://deepsource.io/blog/git-branch-naming-conventions/)
* Commit changes related to your feature and push them to GitHub. You can push changes to your feature branch at any time.
* When you're ready to have your work reviewed, create a pull-request on GitHub.
* If you want guidance on code or content, create a draft pull-request and request a review from a project maintainer.
* When your pull request is ready, you _must_ request a review from one of the project maintainers. Pull requests without an approving review will not be accepted.
* Make changes or respond to comments in your pull-request reviews. New commits pushed to your branch will update the pull-request.
* When your pull request is approved, the reviewer will merge your branch into main and may delete your branch from GitHub.
  * To remove deleted feature branches from your local repository run `git remote prune origin`.
  * Do not attempt to push additional commits to a merged pull-request. Instead, start a new feature branch and issue a new pull request.
* Remember to update and branch off of `main` whenever you start a new feature, e.g., `git checkout main; git pull origin main; git checkout -b a-new-feature`.


## Code style and standards

New code should follow the tidyverse [style guide](https://style.tidyverse.org). You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

In addition, these standards must be adhered to with every contribution.

- No absolute file paths. All paths must be relative to the root project directory.
- No API keys, passwords, credentials, or other secrets. We suggest you use [keyring](https://cran.r-project.org/web/packages/keyring/index.html) package to manage credentials. 
- All commit messages, code comments, and other documentation must be professional and informative. 
- All code should be documented, describing what the code does, a description of parameters, returned values, and side effects. 

In addition, we highly suggest you take the time to set up commit [signing](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits) with GPG.  

## Code of Conduct

Please note that the ghg-cprg project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
