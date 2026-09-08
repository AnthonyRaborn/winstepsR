# Example Winsteps output

`pfile_example.out` is a genuine `PFILE=` written by Winsteps on Windows,
kept so that the parsing in `winsteps_read_person_output()` is tested against
the real layout rather than a reconstruction of it. It shows the details that
are easy to get wrong from documentation alone: the leading comment line, the
column-name line commented out with `;` (so the first name arrives as
`;ENTRY`), values written without a leading zero (`.72`, not `0.72`), and a
trailing blank line.

**The measures in it are not a valid estimation.** It came from the W2 probe,
which fed Winsteps a deliberately malformed data file -- a response block wider
than `NI` declared -- to confirm that Winsteps accepts such a file silently.
Use this file for format, never as an example of what a correct run produces.
