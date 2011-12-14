#lang racket
(length (regexp-match* #px"\\w+" (current-input-port)))