#lang at-exp racket/base

(require "../../main.rkt")

(define time.h-source
   @program{
       struct tm {  
           int tm_sec;
           int tm_min;
           int tm_hour;
           int tm_mday;
           int tm_mon;
           int tm_year;
           int tm_wday;
           int tm_yday;
           int tm_isdst;
       };
   })

(define time.h
  (make-header
   @program{
       struct tm {  
           int tm_sec;
           int tm_min;
           int tm_hour;
           int tm_mday;
           int tm_mon;
           int tm_year;
           int tm_wday;
           int tm_yday;
           int tm_isdst;
       };
   }))

(define windows.h
  (make-header
   @program[#:typedef (SHORT WORD DWORD BOOL CHAR WCHAR
                       MOUSE_EVENT_RECORD
                       MENU_EVENT_RECORD
                       FOCUS_EVENT_RECORD)]{
      typedef struct _COORD {
        SHORT X;
        SHORT Y;
      } COORD;
 
      typedef struct _SMALL_RECT {
        SHORT Left;
        SHORT Top;
        SHORT Right;
        SHORT Bottom;
      } SMALL_RECT;
 
      typedef struct _CONSOLE_SCREEN_BUFFER_INFO {
        COORD dwSize;
        COORD dwCursorPosition;
        WORD wAttributes;
        SMALL_RECT srWindow;
        COORD dwMaximumWindowSize;
      } CONSOLE_SCREEN_BUFFER_INFO;
 
      typedef struct _KEY_EVENT_RECORD {
        BOOL bKeyDown;
        WORD wRepeatCount;
        WORD wVirtualKeyCode;
        WORD wVirtualScanCode;
        union {
          WCHAR UnicodeChar;
          CHAR AsciiChar;
        } uChar;
        DWORD dwControlKeyState;
      } KEY_EVENT_RECORD;
 
      typedef struct _WINDOW_BUFFER_SIZE_RECORD {
        COORD dwSize;
      } WINDOW_BUFFER_SIZE_RECORD;
 
      typedef struct _INPUT_RECORD {
        WORD EventType;
        union {
          KEY_EVENT_RECORD KeyEvent;
          MOUSE_EVENT_RECORD MouseEvent;
          WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
          MENU_EVENT_RECORD MenuEvent;
          FOCUS_EVENT_RECORD FocusEvent;
        } Event;
      } INPUT_RECORD;
   }))

(define time
  (compile-header time.h (system-compiler #:include<> '("time.h"))))

(define windows
  (compile-header windows.h (system-compiler #:include<> '("windows.h" "IpTypes.h") gcc)))
