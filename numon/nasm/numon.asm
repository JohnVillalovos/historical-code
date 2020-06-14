# Copyright 2020 by John L. Villalovos

# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, version 3 of the License.

# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.

#  You should have received a copy of the GNU General Public License along with
#  this program.  If not, see <https://www.gnu.org/licenses/>.

# Program to enable the NUM LOCK state on the keyboard.

    BITS 16                                 ; This is a 16 bit executable for DOS
    ORG 0x100                               ; .COM files start at 0x100

    SECTION .text

    mov     ax,BIOS_DATA_AREA       ; Set DS to the
    mov     ds,ax                   ; BIOS Data area
    mov     ax,[KB_FLAG_OFFSET]     ; Get the keyboard state flag
    or      ax,NUM_SHIFT            ; Set the Num Lock state to on
    mov     [KB_FLAG_OFFSET],ax     ; Save the state

    mov     ax,EXIT_SUCCESS         ; Exit with success
    int     0x21


# Program constants
BIOS_DATA_AREA  EQU     0x0040
KB_FLAG_OFFSET  EQU     0x17
NUM_SHIFT       EQU     0x20

EXIT_SUCCESS    EQU     0x4c00
