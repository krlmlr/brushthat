## brushthat 0.0-3 (2017-06-05)

- Radio buttons change the current result or call stack entry, regression introduced during release of 0.0-1.


## brushthat 0.0-2 (2017-06-04)

- Show only 10 test results by default.


## brushthat 0.0-1 (2017-06-04)

Initial release.

- Showing results as radio buttons, successes not shown by default.
    - If no results, show placeholder text.
- Rerunning all tests with "Run test" button.
- Filtering by result types.
- Restricting maximum number of results shown.
- Escape hatch to drop into temporary shell.
- Showing call stack for currently selected result as radio buttons.
    - Navigating call stack opens corresponding source file.
- Showing error description as label.
- "Done" button.
- Transparent overlay indicates that the app cannot process input:
    - While running tests.
    - When shell is active.
