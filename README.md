# Working with Git in RStudio

This guide helps you set up Git in RStudio, generate an SSH key, add it to GitHub, clone a repository, and start using Git within RStudio to manage your project.

### Prerequisites
- Ensure R and RStudio are installed on your system.
- A GitHub account.

### Step 1: Check and Install Git
1. Open RStudio.
2. Navigate to _Tools > Global Options > Git/SVN_. If Git is not installed, RStudio will prompt you to install it.
3. Follow the instructions to install Git and restart RStudio after installation.

### Step 2: Generate SSH Key in RStudio
1. In RStudio, go to _Tools > Global Options > Git/SVN_.
2. Click on 'Create SSH Key'. Optionally, you can add a passphrase for additional security.
3. Once the key is generated, click on 'View public key', and copy it.

### Step 3: Add SSH Key to GitHub
1. Log in to your GitHub account.
2. Go to Settings (click your profile icon > Settings).
3. Navigate to _SSH and GPG keys_ and click on 'New SSH key'.
4. Paste your copied SSH public key from RStudio into the key field.
5. Provide a descriptive title for the key and click 'Add SSH key'.

### Step 4: Clone Repository Using SSH
1. Go to the GitHub repository you want to clone, e.g., `https://github.com/LucasMolleman/CulturalSystems/`.
2. Click the 'Code' button and switch to 'SSH'. Copy the SSH URL provided.
3. Open Terminal (or Command Prompt) and change the directory to where you want to clone the repository. Use `cd path/to/your/folder`.
4. Execute `git clone` followed by the SSH URL you copied, e.g., `git clone git@github.com:LucasMolleman/CulturalSystems.git`.
5. This clones the repository to your specified location.

### Step 5: Open the R Project
- Navigate to the cloned repository folder and open the `.Rproj` file to start working in RStudio.

### Step 6: Use Git in RStudio to Commit Changes
1. Once you open the `.Rproj` file, the Git tab will appear in the upper right pane of RStudio.
2. Make changes to your files as needed, and save them. 
3. When you're ready to commit:
    - Go to the Git tab, and you'll see a list of modified files.
    - Check the boxes beside the files you want to commit.
    - Click on 'Commit'. This opens a new window.
4. In the commit window:
    - Review your changes.
    - Enter a commit message describing the changes.
    - Click 'Commit'.

## Pushing and Pulling

Two crucial operations when working with Git repositories are *pushing* and *pulling*. These operations help you synchronize your local repository with the remote repository on GitHub.

#### Pushing Changes to GitHub
After committing your changes locally, you'll want to share them with your team or update your remote repository on GitHub. This is done by pushing your commits.

How to Push:
1. Go to the Git tab in RStudio.
2. Click on the 'Push' button (upward arrow icon). This is usually next to the 'Pull' button.
3. If you have committed all your changes, RStudio will push them to the remote repository.
4. Enter your credentials if prompted.

If the push was successful, your changes will now be visible on GitHub in the remote repository.

#### Pulling Changes from GitHub
Before you start working, especially in collaborative projects, you should pull the latest changes from GitHub. This ensures that your local repository is up to date.

How to Pull:
1. Go to the Git tab in RStudio.
2. Click on the 'Pull' button (downward arrow icon). This is usually next to the 'Push' button.
3. RStudio will fetch the latest changes from the remote repository and merge them with your local repository.
4. Resolve any conflicts if they arise during the merge.

By pulling regularly, you reduce the likelihood of merge conflicts and stay updated with the latest work from your collaborators.

#### Best Practices
- **Commit Often**: Make frequent, small commits that encapsulate specific functionalities or bug fixes. This makes it easier to track changes and understand the project's history.
- **Pull Regularly**: Especially in a team setting, pulling changes from the remote repository often helps catch integration issues early.
- **Push At Logical Points**: Push your changes to GitHub once you reach a logical stopping point, such as completing a feature or fixing a bug, ensuring that the remote repository stays current with your local progress.

## Working with Branches in Git

Branching is a core concept in Git that allows you to diverge from the main line of development and work independently without affecting the main line. This is particularly useful in a team setting where different features or fixes can be worked on simultaneously.

#### Why Use Branches?
- **Feature Development**: Work on new features without disturbing the main codebase.
- **Experimentation**: Try out ideas in a contained environment.
- **Bug Fixes**: Address bugs in a separate branch to allow for focused fixes.

#### Creating a Branch in RStudio
1. In RStudio, go to the Git tab in the upper right pane.
2. Click on the 'New Branch' icon, typically represented with a branch or fork symbol.
3. In the dialog that appears, enter a name for your new branch. It's best to use a name that reflects the purpose of the branch (e.g., `feature-login` or `fix-headerbug`).
4. Optionally, you can choose to check out (switch to) the new branch immediately after creation. Click 'Create'.

#### Switching Between Branches
- To switch to another branch, click on the branch name at the top of the Git pane in RStudio. This will display a list of all branches in your repository. Click on the branch you wish to switch to.

#### Merging Branches
After you've completed work on your branch (e.g., finished a feature or fix), you'll want to merge your changes back into the main branch, often called `master` or `main`.
1. Switch to the branch you want to merge into (usually `main` or `master`).
2. Go to the Git tab and click on the 'More' option, typically represented by an ellipsis `...` or a gear icon.
3. Select 'Merge Branch' from the options.
4. In the dialog that appears, select the branch you wish to merge into the current branch and click 'Merge'.

#### Handling Merge Conflicts
- Sometimes, when you attempt to merge branches, you may encounter merge conflicts. These conflicts occur when changes in one branch overlap with changes in another, and Git can't automatically resolve which changes to keep.
- RStudio will indicate files with conflicts. You'll need to manually edit these files to resolve the conflicts, choosing which changes to keep.
- After resolving all conflicts, commit the changes to complete the merge process.

#### Deleting Branches
Once a branch's changes have been successfully merged and it's no longer needed, you can delete the branch to clean up your repository.
1. To delete a branch in RStudio, switch to a different branch (you cannot delete the branch you are currently on).
2. Then, go to the Git tab, click on the 'More' option, and select 'Shell...' to open a terminal.
3. In the terminal, you can delete the branch locally with `git branch -d branch-name` and remotely with `git push origin --delete branch-name`.

#### Best Practices with Branches
- **Naming Conventions**: Adopt a consistent naming convention for branches (e.g., `feature/`, `fix/`, `chore/`), making it easier to understand their purpose at a glance.
- **Keep Branches Short-lived**: Try to merge branches back into the main branch as soon as the work is completed and tested. This reduces the complexity and effort required to merge changes.
- **Regularly Pull From the Main Branch**: While working on a long-lived branch, periodically pull changes from the main branch to minimize merge conflicts.

By effectively using branches, you can keep your repository organized, make collaborative development smoother, and ensure that the main line of development remains stable.

