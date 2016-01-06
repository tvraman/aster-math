;;;   -*- Syntax: Common-Lisp; Package: tts;  Mode: LISP -*-    ;;;
;;; acss.lisp -- Common Lisp Implementation of Aural CSS
;;; $Author: tv.raman.tv $
;;; Description: Aural CSS
;;; Keywords: AsTeR, Emacspeak, Audio Desktop
;;; { Copyright:

;;; Copyright (C) 2011 -- 2016, T. V. Raman<tv.raman.tv@gmail.com>
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING. If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; }m
(in-package :afl)
(proclaim '(optimize (compilation-speed 0) (safety 1) (speed 3)))

(defstruct acss
  "Aural CSS"
  family gain
  left-volume right-volume
  average-pitch pitch-range
  stress richness
  punctuations
  ) 

