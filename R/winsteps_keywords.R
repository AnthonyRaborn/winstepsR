# Known Winsteps control-file keywords, used only to catch a typo in
# `estimation` or `extra` before it reaches Winsteps -- which silently
# ignores an unrecognized keyword rather than erroring, so a typo there
# produces a control file that looks right and quietly is not.
#
# Compiled from the Winsteps control-variable reference (winsteps.com/winman/
# controlusage.htm and the "Control Variables by function" index), current as
# of Winsteps' documented keyword set. This list is not read from Winsteps
# itself, so a keyword introduced in a newer Winsteps release than this list
# will warn here even though it is valid -- hence a warning, not an error.
winsteps_known_keywords <- c(
  # Data file layout
  "DATA", "DELIMITER", "FORMAT", "ILFILE", "INUMB", "ITEM1", "ITLEN",
  "MFORMS", "NAME1", "NAMLEN", "NI", "PLFILE", "SEPARATOR", "XWIDE", "@FIELD",
  # Data selection and recoding
  "ALPHANUM", "CODES", "CUTHI", "CUTLO", "EDFILE", "IREFER", "IWEIGHT",
  "KEYFROM", "KEYSCR", "MAKEKEY", "MISSCORE", "NEWSCORE", "PWEIGHT",
  "RESCORE", "RESFRM",
  # Items: deleting, anchoring and selecting
  "IAFILE", "IANCHQU", "IDELETE", "IDELQU", "IDFILE", "IDROPEXTR", "ISELECT",
  # Person: deleting, anchoring and selecting
  "PAFILE", "PANCHQU", "PDELETE", "PDELQU", "PDFILE", "PDROPEXTR", "PSELECT",
  # Rating scales, partial credit items and polytomous response structures
  "GROUPS", "GRPFROM", "ISGROUPS", "MODELS", "MODFROM", "STKEEP",
  # Category structure: anchoring, labeling, deleting
  "CFILE", "CLFILE", "SAFILE", "SAITEM", "SANCHQU", "SDELQU", "SDFILE",
  # Measure origin, anchoring and user-scaling
  "UAMOVE", "UANCHOR", "UASCALE", "UDECIMALS", "UEXTREME", "UIMEAN", "UMEAN",
  "UPMEAN", "USCALE",
  # Output table selection and format
  "ASCII", "FORMFEED", "HEADER", "ITEM", "LINELENGTH", "MAXPAGE", "PERSON",
  "TABLES", "TFILE", "TITLE", "TOTALSCORE", "WEBFONT",
  # Output tables, files and graphs: specific controls
  "ASYMPTOTE", "BOXSHOW", "BYITEM", "CATREF", "CHART", "CMATRIX", "CURVES",
  "DISCRIM", "DISTRT", "EQFILE", "FITHIGH", "FITI", "FITLOW", "FITP",
  "FRANGE", "HIADJ", "IMAP", "IPEXTREME", "ISORT", "ISUBTOTAL", "LOWADJ",
  "MNSQ", "MATRIX", "MRANGE", "MTICK", "NAMLMP", "OSORT", "OUTFIT", "PMAP",
  "PSORT", "PSUBTOTAL", "PVALUE", "STEPT3", "T7OPTIONS", "TRPOTYPE",
  "UCOUNT", "W300",
  # Output file format control
  "CSV", "HLINES", "QUOTED",
  # Estimation, operation and convergence control
  "ANCESTIM", "CONVERGE", "EXTRSCORE", "LCONV", "LOCAL", "MJMLE", "MPROX",
  "MUCON", "NORMAL", "PAIRED", "PTBISERIAL", "RCONV", "REALSE", "STBIAS",
  "TARGET", "WHEXACT",
  # Program operation
  "BATCH", "FSHOW", "SPFILE",
  # Secondary (post-hoc) processing
  "DIF", "DPF", "G0ZONE", "G1ZONE", "PRCOMP", "SICOMPLETE", "SIEXTREME",
  "SINUMBER", "SVDDEPTH", "SVDMAX", "SVDTYPE",
  # Output files
  "AGREEFILE", "DISFILE", "EFILE", "GRFILE", "GUFILE", "ICORFILE", "IFILE",
  "IPMATRIX", "ISFILE", "PCORFIL", "PFILE", "RFILE", "SCOREFILE", "SFILE",
  "SIFILE", "SVDFILE", "TCCFILE", "TRPOFILE", "XFILE", "KEYFORM", "LOGFILE",
  "OFILE"
)

# Keyword families indexed by a trailing number (KEY1=, KEY2=, ...; IVALUE1=,
# IVALUE2=, ...), which winsteps_known_keywords above cannot list one by one.
winsteps_known_keyword_patterns <- c("^KEY[0-9]+$", "^IVALUE[0-9]+$",
                                     "^T1I[0-9]*#?$", "^T1P[0-9]*#?$")

is_known_winsteps_keyword <- function(key) {
  key <- toupper(key)
  if (key %in% winsteps_known_keywords) return(TRUE)
  if (any(vapply(winsteps_known_keyword_patterns, grepl, logical(1), x = key))) {
    return(TRUE)
  }
  # Winsteps accepts an unambiguous abbreviation of a keyword (e.g. UDECIM
  # for UDECIMALS), so a prefix matching exactly one known keyword also
  # counts as recognized. A prefix matching several (or none) does not.
  sum(startsWith(winsteps_known_keywords, key)) == 1
}

# The closest known keyword to an unrecognized one, or NULL if none is close
# enough to suggest -- e.g. "UDECIM" -> "UDECIMALS".
winsteps_keyword_suggestion <- function(key) {
  hit <- agrep(toupper(key), winsteps_known_keywords, max.distance = 0.25,
              ignore.case = TRUE, value = TRUE)
  if (length(hit) == 0) NULL else hit[1]
}

# Warn (not error) about any key not matching a known Winsteps keyword.
# A soft check: an unlisted keyword is not necessarily wrong -- it may be
# valid in a Winsteps release newer than this list -- but is worth a second
# look, since Winsteps itself will not report the same mistake.
check_known_keywords <- function(keys) {
  bad <- unique(keys[!vapply(keys, is_known_winsteps_keyword, logical(1))])
  for (key in bad) {
    suggestion <- winsteps_keyword_suggestion(key)
    warning(
      "\"", key, "\" is not a keyword winstepsR recognizes",
      if (!is.null(suggestion)) paste0(" (did you mean \"", suggestion, "\"?)") else "",
      ". It will be written to the control file, but Winsteps silently ",
      "ignores an unrecognized keyword rather than erroring, so if this is ",
      "a typo it will have no effect. Check the spelling against the ",
      "Winsteps manual.", call. = FALSE
    )
  }
  invisible(keys)
}
