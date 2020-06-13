#lang racket

(module ffi-part racket
  (provide get-all-process-name-and-id
           get-process-by-name
           write-process-memory
           get-module-base-by-image-name
           bytes/utf-16->string)
  (require ffi/unsafe
           ffi/unsafe/define
           ffi/winapi)

  (define-syntax-rule (_wfun type ...)
    (_fun #:abi winapi type ...))

  (define-ffi-definer define-kernel32 (ffi-lib "kernel32.dll"))
  (define-ffi-definer define-psapi (ffi-lib "psapi.dll"))

  (define _HANDLE _uintptr) ;(_cpointer/null 'OBJECT))

  (define MAX_PATH 260)
  (define MAX_MODULE 128)

  (define-cstruct
    _PROCESSENTRY32W
    ([dwSize                _uint32]
     [cntUsage              _uint32]
     [th32ProcessID         _uint32]
     [th32DefaultHeapID     (_cpointer/null _uint64)]
     [th32ModuleID          _uint32]
     [cntThreads            _uint32]
     [th32ParentProcessID   _uint32]
     [pcPriClassBase        _int32]
     [dwFlags               _uint32]
     [szExeFile             (_array/list _uint8 (* 2 MAX_PATH))]))

  (define-cstruct
    _MODULEINFO
    ([lpBaseOfDll           _uintptr]
     [SizeOfImage           _uint32]
     [EntryPoint            _uintptr]))


  (define-kernel32 CreateToolhelp32Snapshot (_wfun _uint32 _uint32 -> _HANDLE))
  (define-kernel32 Process32FirstW (_wfun _HANDLE _PROCESSENTRY32W-pointer/null -> _bool))
  (define-kernel32 Process32NextW  (_wfun _HANDLE _PROCESSENTRY32W-pointer/null -> _bool))
  (define-kernel32 CloseHandle     (_wfun _HANDLE -> _bool))
  (define-kernel32 OpenProcess     (_wfun _uint32 _bool _uint32 -> _HANDLE))
  (define-kernel32 WriteProcessMemory
    (_wfun _HANDLE _uintptr _pointer _uintptr _uintptr -> _bool))
  (define-psapi    EnumProcessModules   (_wfun _HANDLE
                                               (modules : (_list o _HANDLE MAX_MODULE))
                                               (_int32 = (* MAX_MODULE (ctype-sizeof _HANDLE)))
                                               (need : (_ptr o _int32))
                                               -> (ret : _bool)
                                               -> (values modules need ret)))
  (define-psapi    GetModuleFileNameExW (_wfun _HANDLE
                                               _HANDLE
                                               (filename : (_bytes/nul-terminated
                                                            o
                                                            (* 2 MAX_PATH)))
                                               (_int32 = MAX_PATH)
                                               -> (ret : _int32)
                                               -> (values filename ret)))
  (define-psapi   GetModuleInformation  (_wfun _HANDLE
                                               _HANDLE
                                               (modinfo : (_ptr o _MODULEINFO))
                                               (_int32 = (ctype-sizeof _MODULEINFO))
                                               -> (ret : _bool)
                                               -> (values modinfo ret)))
  

  (define (bytes/utf-16->string input)
    (let ([convert (bytes-open-converter "platform-UTF-16" "platform-UTF-8")])
      (let-values ([(utf-8 len state) (bytes-convert convert input)])
        (begin0 (string-replace (bytes->string/utf-8 utf-8) "\0" "")
          (bytes-close-converter convert)))))

  (define (iterate-all-process pprocess handle now-list)
    (if (Process32NextW handle pprocess)
        (begin
          (iterate-all-process
           pprocess handle
           (cons
            (cons
             (bytes/utf-16->string
              (list->bytes
               (PROCESSENTRY32W-szExeFile
                (ptr-ref pprocess _PROCESSENTRY32W))))
             (PROCESSENTRY32W-th32ProcessID
              (ptr-ref pprocess _PROCESSENTRY32W)))
            now-list)))
        now-list))

  (define (get-all-process-name-and-id)
    (let
        ([pprocess (make-PROCESSENTRY32W
                    (ctype-sizeof _PROCESSENTRY32W)
                    0 0 #f 0 0 0 0 0
                    (make-list (* 2 MAX_PATH) 0))]
         [snapshot (CreateToolhelp32Snapshot 2 0)])
      (begin0
          (if (Process32FirstW snapshot pprocess)
              (iterate-all-process
               pprocess
               snapshot
               (list (cons
                      (bytes/utf-16->string
                       (list->bytes
                        (PROCESSENTRY32W-szExeFile
                         (ptr-ref pprocess _PROCESSENTRY32W))))
                      (PROCESSENTRY32W-th32ProcessID
                       (ptr-ref pprocess _PROCESSENTRY32W)))))
              empty)
        (CloseHandle snapshot))))

  (define (get-process-by-name name)
    (for/or ([n-i (get-all-process-name-and-id)])
      (if (equal? name (car n-i)) (cdr n-i) #f)))

  (define (get-module-base-by-image-name pid image)
    ; #x0038: PROCESS_VM_WRITE | PROCESS_VM_READ | PROCESS_VM_CONTROL
    (let*-values
        ([(process-handle) (OpenProcess #x0038 #f pid)]
         [(module-handles nd ret) (EnumProcessModules process-handle)]
         [(target-module)
          (for/or ([module-handle module-handles])
            (let*-values
                ([(name-bytes ret) (GetModuleFileNameExW process-handle module-handle)]
                 [(name-string) (bytes/utf-16->string name-bytes)])
              (if (and
                   (>= (string-length name-string) (string-length image))
                   (equal? "ffxiv_dx11.exe"
                           (substring name-string
                                      (- (string-length name-string)
                                         (string-length image)))))
                  module-handle
                  #f)))])
      (begin0
          (if target-module
              (let-values ([(modinfo ret) (GetModuleInformation process-handle target-module)])
                (MODULEINFO-lpBaseOfDll modinfo))
              #f)
        (CloseHandle process-handle))))

  (define (write-process-memory pid address content)
    ; #x0038: PROCESS_VM_WRITE | PROCESS_VM_READ | PROCESS_VM_CONTROL
    (let* ([process-handle (OpenProcess #x0038 #f pid)]
           [len    (length content)]
           [memptr (malloc len)]
           [marray (ptr-ref memptr (_array _uint8 len))])
      (for ([i (in-range len)])
        (array-set! marray i (list-ref content i)))
      (if process-handle
          (begin0
              (WriteProcessMemory process-handle
                                  address
                                  memptr
                                  len
                                  0)
            (CloseHandle process-handle))
          #f))))

(module gui-part racket
  (provide main)
  (require (submod ".." ffi-part))
  (require racket/gui)

  (define (patch)
    ;; by @Bluefissure
    (let* ([pid  (or (get-process-by-name "ffxiv_dx11.exe") 0)]
           [base (get-module-base-by-image-name pid "ffxiv_dx11.exe")])
      (and (write-process-memory pid
                                 (+ base #x8450ed)
                                 '(#x90 #x90))
           (write-process-memory (or (get-process-by-name "ffxiv_dx11.exe") 0)
                                 (+ base #x845108)
                                 '(#x90 #x90)))))

  (define (recover)
    (let* ([pid  (or (get-process-by-name "ffxiv_dx11.exe") 0)]
           [base (get-module-base-by-image-name pid "ffxiv_dx11.exe")])
      (and (write-process-memory (or pid 0)
                                 (+ base #x8450ed)
                                 '(#x75 #x33))
           (write-process-memory (or pid 0)
                                 (+ base #x845108)
                                 '(#x74 #x18)))))
  
  (define (main)

    (define test-frame%
      (class frame%
        (define/override (on-subwindow-char r ke) (#f))
        (super-new)))
    
    (define frame (instantiate dialog% ("QAQ")))
    (define msg (new message%
                     [parent frame]
                     [label "————"]))

    (new button%
         [parent frame]
         [label "Patch"]
         ; Callback procedure for a button click:
         [callback (lambda (button event)
                     (send msg set-label (if (patch) "Okey!" "Fail.")))])

    (new button%
         [parent frame]
         [label "Revover"]
         ; Callback procedure for a button click:
         [callback (lambda (button event)
                     (send msg set-label (if (recover) "Okey!!" "Fail..")))])

    (send frame show #t)))

(require 'ffi-part)
(require 'gui-part)

(main)
