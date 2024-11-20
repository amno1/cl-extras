;; packages.cl
;; Copyright (C) 2024  Arthur Miller

;; Author: Arthur Miller <arthur.miller@live.com>
;; Version: 0.0.1
;; Date: 2024-11-20
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:

;; 

;;; Code:

(in-package :cl-user)

(uiop:define-package "CL-EXTRAS"
  (:use :cl)
  (:nicknames :ext)
  (:shadow #:defun)
  (:export #:import-from))


;;; packages.cl ends here
