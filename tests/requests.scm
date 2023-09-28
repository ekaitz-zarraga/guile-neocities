;; tests/requests --- neocities requests tester -*- coding: utf-8 -*-
;;
;; Copyright (C) 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
;;
;; Author: Ekaitz Zarraga <ekaitz@elenq.tech>
;;
;; This file is part of guile-neocities.
;;
;; guile-neocities is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; guile-neocities is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with guile-neocities; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(define-module (tests requests)
  #:use-module (neocities requests)
  #:use-module (web uri)
  #:use-module (srfi srfi-64))

(test-begin "request-url")
(define url (neocities-url "neocities.org"))
(test-assert (uri? url))
(test-end "request-url")
