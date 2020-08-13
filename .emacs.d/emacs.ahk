; Recommended, but not required:
SendMode Input
#NoEnv
#SingleInstance force

#Include <dual/dual>
dual := new Dual

#Include <dual/defaults>

#If true ; Override defaults.ahk. There will be "duplicate hotkey" errors otherwise.

; Steve Losh shift buttons.
;*LShift::
;*LShift UP::dual.combine(A_ThisHotkey, "(")
;*RShift::
;*RShift UP::dual.combine(A_ThisHotkey, ")")

; BigCtrl-like.
*Space::
*Space UP::dual.combine("RCtrl", A_ThisHotkey)

; Map Capslock to C-c
Capslock::^c

#If

*ScrollLock::dual.reset()
