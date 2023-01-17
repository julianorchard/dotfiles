;;;;  ico_getter.ahk  ---  Scrape ICO numbers and find lift details

;; Copyright (c) Wessex Lift Co. Ltd. <marketing@wessexlifts.co.uk>

;;;; Description:

;; A small AutoHotkey utility, borne of a lack of descriptions of
;; folders in certain folders of Wessex drives.

;; The script searches for an ICO number and tries to then input it
;; into our CRM system. It then automatically finds out what type of
;; lift this is.

;; Bound to `Start + Alt + t`, currently.

;; CRM needs to be open and focussed on for this to work...

;; TODO: Script should tell you if images are in the folder.

;;;; License:

;; This is licensed under the MIT License.

;;;; Code:

inputCRM(i) {
;; This function does the ugly handling of the entering of information
;; into our CRM system. This'll involve a lot of keyboard inputs and
;; mouse movements.

;; F10 opens the search window
  Send, {F12}
  Sleep, 500

;; Send Ctrl + f to find, then tab
  Send, ^{f}
  Sleep, 500
  Send, {Tab 18}
  Sleep, 500

;; Enter the ICO number
  Send, % i
  Sleep, 500
  Send, {Enter}
  ;; TODO: No record found error handle (if winactive(err window))
  Sleep, 2000

;; F5, get to Quotes Data Entry Page... could we do it from this page anyway...
;; dunno
  Send, {F5}
  Sleep, 2000

;; Send 31 tabs to get to the right field...
  Send, {Tab 31}
  Sleep, 1000

;; Copy to clipboard, check if the model contains LR (or whatever, eventually)
  clipboard := ""
  Sleep, 500
  Send, ^+{Right}
  Sleep, 500
  Send, ^{c}
  ClipWait
  if (InStr(clipboard, "LR"))
  {
    Return true
  }
  else
  {
    Return false
  }

;; Testin
  ; KeyWait, z, D
}

!#t::
  folder_to_search=P:\Instal\Site Photos\Stannah\*
  Loop, Files, %folder_to_search%, D
    if !(SubStr(A_LoopFileName, 5, 6) ~= "^\d{6}$")
    {
      not_parsed = % not_parsed "- " A_LoopFileName "`n"
      Continue
    } else {

      ;; TODO: Regex this bit
      file_list = % file_list SubStr(A_LoopFileName, 5, 6) "`n"

      ;; here, we start doing the inputting into CRM bit
      ; if (inputCRM(SubStr(A_LoopFileName, 5, 6)))
      ; {
      ;   matched_list = % matched_list A_LoopFileName "`n"
      ; }

    }
  ; FileDelete, C:\Users\jorchard\tester.txt
  ; FileAppend, % matched_list, ./tester.txt
  FileDelete, C:\Users\jorchard\all.txt
  FileAppend, % file_list, ./all.txt

  ;; Notify of errors
  MsgBox, % "The following folders not be identified by this command: `n`n" not_parsed
Return

;;;; Provide ico_getter.ahk
