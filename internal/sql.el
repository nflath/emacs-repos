;;; Adding some keywords for Oracle PL/SQL

(font-lock-add-keywords 'sql-mode '(("\\(VARCHAR2\\)" 1 font-lock-type-face prepend)))
(font-lock-add-keywords 'sql-mode '(("\\(MINUS\\)" 1 font-lock-type-face prepend)))
