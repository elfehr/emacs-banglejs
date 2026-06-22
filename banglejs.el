;;; banglejs.el --- BangleJS serial terminal  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026  Elfehr
;;
;; Author: Elfehr <2046083-elfehr@users.noreply.gitlab.com>
;; Keywords: terminals, tools
;; Package-Requires: nil
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Interacts with the Javascript interpreter of an Espruino device via
;; a serial or TCP port. This is meant as a replacement of the
;; Espruino WebIDE.
;;
;;; Usage:
;;
;; Connect the device to a serial port or a TCP socket, for example
;; with https://github.com/Jakeler/ble-serial. You must disconnect the
;; device from the normal bluetooth interface before it can be
;; recognized.
;;
;;;; Serial backend:
;;
;; E.g.: `ble-serial --dev XX:XX:XX:XX:XX:XX --read-uuid
;; 6e400003-b5a3-f393-e0a9-e50e24dcca9e --write-uuid
;; 6e400002-b5a3-f393-e0a9-e50e24dcca9e [--port /tmp/ttyBLE]'
;;
;; Set `banglejs-term-start-function' to `banglejs-start-serial-term'
;; and `banglejs-serial-device' to the path to the serial device.
;;
;; The terminal is based on serial-terminal and uses term-mode. It
;; tends to leave the device busy when the process is killed and
;; doesn't supports fontification, so the TCP method is preferred.
;;
;;;; TCP backend:
;;
;; E.g.: `ble-serial --dev XX:XX:XX:XX:XX:XX --read-uuid
;; 6e400003-b5a3-f393-e0a9-e50e24dcca9e --write-uuid
;; 6e400002-b5a3-f393-e0a9-e50e24dcca9e --expose-tcp-port 4002
;; [--expose-tcp-host 127.0.0.1]'
;;
;; Set `banglejs-term-start-function' to `banglejs-start-tcp-term' and
;; `banglejs-tcp-program' to the (host . port) combination.
;;
;; `banglejs-tcp-program' can also be a program name, in which case
;; the list of arguments must be set in `banglejs-tcp-program-args'.
;; E.g.:
;; `(setq banglejs-tcp-program "telnet"
;;        banglejs-tcp-program-args '("127.0.0.1" "4002"))'
;; `(setq banglejs-tcp-program "socat"
;;        banglejs-tcp-program-args '("-dd" "tcp:localhost:4002" "-"))'
;;
;; The terminal uses the major mode `banglejs-tcp-term-mode', based on
;; Comint.
;;
;;;; Usage for both backends:
;;
;; Call `banglejs-term' to start the terminal or display its buffer.
;; You can set `banglejs-term-window-direction' to start the terminal
;; in a new window in an explicit direction.
;;
;; From any buffer, you can call:
;;
;; - `banglejs-send-region', `banglejs-send-defun',
;;   `banglejs-send-line': send code to the interpreter.
;; - `banglejs-write-region-to-file': query a file name to upload the
;;   region or buffer to the device's storage.
;; - `banglejs-read-file': download a file from the device's storage
;;   and display it in a new buffer.
;; - `banglejs-delete-file': erase a file from device's storage.
;;
;; For convenience, the minor mode `banglejs-mode' binds some of those
;; functions.
;;
;;
;;;; Starting ble-serial automatically
;;
;; You can set `banglejs-ble-serial-cmd' to the shell command starting
;; ble-serial or any equivalent program. The program is called from
;; the function `banglejs-ble-serial-start'. That function blocks
;; until it receives an output that indicates the startup is finished,
;; which is set by `banglejs-ble-serial-success-regexp'.
;; `banglejs-ble-serial-cmd-regexp' should be set to a pair of regexp
;; matching the executable and arguments of `banglejs-ble-serial-cmd',
;; in order not to start ble-serial if it is already running,
;; potentially externally.
;;
;; This allows adding the function to the hook running before
;; banglejs-term:
;; `(add-hook 'banglejs-term-startup-hook #'banglejs-ble-serial-start)'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'map)
(require 'windmove)
(require 'comint)
(require 'term)
(provide 'banglejs)

(defgroup banglejs nil
  "BangleJS serial terminal"
  :group 'programming)



;; Common functions for any backend

(defcustom banglejs-term-start-function #'banglejs-start-tcp-term
  "Function to start the terminal connected to the Espruino interpreter.
Can be one of:
- `banglejs-start-tcp-term': comint-mode terminal calling
  `banglejs-tcp-program'.
- `banglejs-start-serial-term': term-mode serial terminal connected to
  `banglejs-serial-device'."
  :type 'function
  :safe (lambda (x) (memq x '(banglejs-tcp-program
                              banglejs-serial-device))))

(defcustom banglejs-term-buffer-name "*Banglejs*"
  "Buffer of the terminal connected to the Espruino interpreter."
  :type 'string
  :safe #'stringp)

(defcustom banglejs-term-window-direction nil
  "Direction in which to open the terminal window.
One of `above', `below', `left' or `right'. If nil, respect
`display-buffer-alist'."
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)
                 (const nil))
  :safe t)

(defcustom banglejs-chunk-size 500
  "Maximium number of characters to write into storage at once."
  :type 'integer
  :safe #'integerp)

(defvar banglejs--files nil
  "List of files on the BangleJS.")

(defvar banglejs-term-startup-hook nil
  "Hook running before `banglejs-term'.")

;;;###autoload
(defun banglejs-term ()
  "Start a terminal connected to the Espruino interpreter.
If a process is already running, simply display the buffer.
The type of terminal is controlled by the variable
`banglejs-term-start-function'."
  (interactive)
  (let* ((buffer (get-buffer-create banglejs-term-buffer-name)))
    (unless (comint-check-proc buffer)
      (run-hooks 'banglejs-term-startup-hook)
      (funcall banglejs-term-start-function buffer))
    (when buffer
      (when banglejs-term-window-direction
        (windmove-display-in-direction banglejs-term-window-direction))
      (display-buffer buffer))))

(defun banglejs-send-command (string &optional no-newline)
  "Send command STRING to the Espruino interpreter."
  (unless (comint-check-proc banglejs-term-buffer-name)
    (error "No BangleJS process"))
  (let* ((process (get-buffer-process banglejs-term-buffer-name))
         (cmd (if no-newline string (concat string "\n"))))
    (process-send-string process cmd)))

(defun banglejs-send-region (begin end)
  "Send the region to the Espruino interpreter."
  (interactive (list (region-beginning) (region-end)))
  (when (region-active-p)
    (pulse-momentary-highlight-region begin end)
    (banglejs-send-command (buffer-substring-no-properties begin end))
    (deactivate-mark)
    (display-buffer banglejs-term-buffer-name)))

(defun banglejs-send-line ()
  "Send the current line to the Espruino interpreter."
  (interactive)
  (banglejs-send-region (pos-bol) (pos-eol)))

(defun banglejs-send-defun ()
  "Send the current definition to the Espruino interpreter."
  (interactive)
  (deactivate-mark)
  (mark-defun)
  (banglejs-send-region (region-beginning) (region-end)))

(defun banglejs-get-command-output (cmd &optional hide)
  "Return output of command CMD, return value excluded.
If HIDE, don't display the output in the terminal."
  (with-current-buffer banglejs-term-buffer-name
    (let ((output (cond
                   ((derived-mode-p 'term-mode)
                    (banglejs-serial-term--get-command-output cmd))
                   ((derived-mode-p 'comint-mode)
                    (banglejs-tcp-term--get-command-output cmd hide)))))
      (seq-subseq output 1 -2)))) ; remove cmd, return value and new prompt

(defun banglejs--get-files ()
  "Get a list of the files on the BangleJS and store it as `banglejs--files'."
  (interactive)
  (setq banglejs--files
        (banglejs-get-command-output
         "print(require('Storage').list(undefined, {sf:false}).join('\\n'))" t))
  banglejs--files)

(defun banglejs--file-prompt (prompt &optional refresh-list)
  "Prompt for a file on the BangleJS."
  (let ((files (or (unless refresh-list banglejs--files)
                   (banglejs--get-files))))
    (completing-read prompt files)))

(defun banglejs-read-file (file &optional hide)
  "Prompt for a file on the BangleJS and returns a buffer containing its content.
With a prefix argument, refresh the cached list of files before prompting.
If HIDE is non-nil, bury the buffer."
  (interactive (list (banglejs--file-prompt "Read file: " current-prefix-arg)))
  (let* ((name (format "%s:%s" (string-trim banglejs-term-buffer-name "*" "*") file))
         (buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (insert
       (string-join
        (banglejs-get-command-output
         (format "print(require('Storage').read('%s'))" file) t)
        "\n")))
    (unless hide
      (display-buffer buffer))
    buffer))

(defun banglejs-write-region-to-file (file)
  "Prompt for a file on the BangleJS and write the region or whole buffer to that
file.
With a prefix argument, refresh the cached list of files before prompting.
The amount of data that can be written at once is limited by the available RAM.
It is controlled by `banglejs-chunk-size'."
  (interactive (list (banglejs--file-prompt "Write to file: " current-prefix-arg)))
  (save-excursion
    (let* ((begin (if (region-active-p) (region-beginning) (point-min)))
           (end (if (region-active-p) (region-end) (point-max)))
           (region (buffer-substring-no-properties begin end))
           (total-bytes (string-bytes region))
           (total-chars (length region))
           (byte-idx 0) (char-idx 0))
      (pulse-momentary-highlight-region begin end)
      (deactivate-mark)
      (while (< char-idx total-chars)
        (let* ((new-idx (+ char-idx banglejs-chunk-size))
               (chunk (substring region char-idx (min new-idx total-chars)))
               ;; escape backslashes, newlines, quotes used in string interpolation
               (string (replace-regexp-in-string "\\\\" "\\\\\\\\" chunk))
               (string (replace-regexp-in-string "\n" "\\\\n" string))
               (string (replace-regexp-in-string "`" "\\\\`" string))
               (cmd (format "require('Storage').write('%s', `%s`, %d, %d)"
                            file string byte-idx total-bytes)))
          (banglejs-get-command-output cmd t) ; wait before sending next chunk
          (setq char-idx new-idx)
          (setq byte-idx (+ byte-idx (string-bytes chunk)))))
      (with-current-buffer (banglejs-read-file file 'hide)
        (unless (string= (buffer-string) region)
          (error "File %s failed to be written correctly" file)))
      (push file banglejs--files))))

(defun banglejs-delete-file (file)
  "Prompt for a file on the BangleJS and erase it.
With a prefix argument, refresh the cached list of files before prompting."
  (interactive (list (banglejs--file-prompt "Erase file: " current-prefix-arg)))
  (when (yes-or-no-p (format "Erase file `%s' from the internal memory? " file))
    (banglejs-send-command
     (format "print(require('Storage').erase('%s'))" file))))


;;;###autoload
(define-minor-mode banglejs-mode
  "Minor mode to interact with the Espruino terminal from the current buffer."
  :lighter " BangleJS"
  :keymap
  `((,(kbd "C-c C-r") . banglejs-send-region)
    (,(kbd "C-c C-l") . banglejs-send-line)
    (,(kbd "C-c C-d") . banglejs-send-defun)
    (,(kbd "C-c C-w") . banglejs-write-region-to-file))

  (if banglejs-mode
      (message "Banglejs-mode activated")
    (message "Banglejs-mode deactivated"))
  (run-hooks 'banglejs-mode-hook))



;; Use a TCP server as terminal (via comint)

(defcustom banglejs-tcp-program '("127.0.0.1" . 4002)
  "The argument PROGRAM passed to `make-comint-in-buffer' to start the terminal.
For example, use \\='(\"127.0.0.1\" . 4002) to open a raw TCP connection on port
4002. It could also be the name of an executable, whose arguments are given by
`banglejs-tcp-program-args'."
  :type '(choice (cons string (choice integer string)) string)
  :safe #'consp) ; host+port is safe, arbitrary executable is not

(defcustom banglejs-tcp-program-args nil
  "A list of additional arguments to `make-comint-in-buffer'.
If `banglejs-tcp-program' is a string, they are the executable's argument."
  :type '(repeat string)
  :safe t)

(defvar banglejs-tcp-term--output nil
  "Variable to accumulate terminal output within a filter function.")
(defvar banglejs-tcp-term--reply-finished nil
  "Variable to signal the output is whole within a filter function.")
(defvar banglejs-tcp-term--hide-output nil
  "Whether the filter function should hide the output.")

(defvar banglejs-tcp-term-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (keymap-set map "M-RET" #'comint-accumulate)
    map)
  "Mode map for `banglejs-tcp-term'.")

(defun banglejs-start-tcp-term (buffer)
  "Start the BangleJS terminal as a Comint process."
  (with-current-buffer buffer
    (apply #'make-comint-in-buffer "Banglejs[tcp]" buffer
           banglejs-tcp-program nil banglejs-tcp-program-args)
    (banglejs-tcp-term-mode)))

(defun banglejs-tcp-term--get-command-output (cmd &optional hide)
  "Return output of command CMD from a Comint process."
  (with-current-buffer banglejs-term-buffer-name
    (add-hook 'comint-preoutput-filter-functions #'banglejs-tcp-term--intercept-output nil t)
    (setq banglejs-tcp-term--output nil)
    (setq banglejs-tcp-term--reply-finished nil)
    (setq banglejs-tcp-term--hide-output hide)
    (banglejs-send-command cmd)
    (while (not banglejs-tcp-term--reply-finished) ; wait for the output hooks
      (sit-for 0.05))
    (remove-hook 'comint-preoutput-filter-functions #'banglejs-tcp-term--intercept-output t)
    (split-string banglejs-tcp-term--output "\r\n")))

(defun banglejs-tcp-term--intercept-output (output)
  "Accumulate terminal output in `banglejs-tcp-term--output', and return
nothing to display if `banglejs-tcp-term--hide-output' is non-nil."
  (setq banglejs-tcp-term--output (concat banglejs-tcp-term--output output))
  (when (or (string-suffix-p "\r\n>" output) ; new prompt: output finished
            (string-match-p "^\n?>$" output)) ; suffix split between different outputs
    (setq banglejs-tcp-term--reply-finished t))
  (if banglejs-tcp-term--hide-output "" output))

(defun banglejs-tcp-term--initialize ()
  "Configure `banglejs-tcp-term-mode'."
  (setq-local comint-process-echoes t)
  (comint-send-input) ; make the first prompt appear
  (comint-kill-region (point-min) (pos-bol))
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-highlight-input nil)
  (setq-local comint-indirect-setup-function #'js-mode)
  (setq-local comment-start "// ")
  (comint-fontify-input-mode 1))

(define-derived-mode banglejs-tcp-term-mode comint-mode "Banglejs[tcp]"
  "Major mode for `banglejs-tcp-term'."
  (banglejs-tcp-term--initialize))



;; Use serial-terminal

(defcustom banglejs-serial-device "/tmp/ttyBLE"
  "Path to the serial port device file, for example `/dev/pts/2'."
  :type 'file
  :safe #'stringp)

(defvar banglejs-serial-term--timeout 1
  "Timeout (in seconds) for `banglejs-serial-term--get-command-output' to wait for
more output.")

(defun banglejs-serial-term--get-command-output (cmd)
  "Return output of command CMD from a serial terminal."
  (interactive)
  (let* ((process (get-buffer-process banglejs-term-buffer-name))
         (old-prompt (marker-position (process-mark process))))
    (banglejs-send-command cmd)
    (while (accept-process-output process banglejs-serial-term--timeout))
    (with-current-buffer banglejs-term-buffer-name
      (split-string
       (buffer-substring-no-properties
        old-prompt (marker-position (process-mark process)))
       "\n"))))

(defun banglejs-start-serial-term (buffer)
  "Start the BangleJS terminal via `make-serial-process'."
  (require 'term)
  (serial-supported-or-barf)
  (let* ((process (make-serial-process
                   :port banglejs-serial-device
                   :speed nil
                   :coding 'no-conversion
                   :name "Banglejs[serial]"
                   :buffer buffer
                   :filter #'term-emulate-terminal
                   :sentinel #'term-sentinel)))
    (with-current-buffer buffer
      (term-mode)
      (term-line-mode)
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (term-send-input)))) ; make the first prompt appear



;; Create the serial port / TCP server

(defcustom banglejs-ble-serial-cmd
  "ble-serial --dev %d --read-uuid %r --write-uuid %w --expose-tcp-port %p"
  "Shell command to connect the Espruino device to a serial port or TCP server.
Any %-sequences will be interpolated according to `banglejs-ble-serial-format'."
  :type 'string
  :safe nil)

(defcustom banglejs-ble-serial-success-regexp "Running main loop!"
  "Regexp output by `banglejs-ble-serial-cmd' to indicate success.
It is used to detect that the serial port / TCP server initialization is
finished."
  :type 'string
  :safe #'stringp)

(defcustom banglejs-ble-serial-cmd-regexp '("ble-serial" "--dev %d")
  "List of two regexp, matching respectively the executable name and invoking
command line of the ble-serial process started by `banglejs-ble-serial-cmd'.
Any %-sequences will be interpolated according to `banglejs-ble-serial-format'.
This is used by `banglejs-ble-serial-start' to check whether ble-serial is
already running, potentially outside emacs. If this variable or its first
element is nil, no check is performed."
  :type '(choice (list string (choice string (const nil)))
                 (const nil))
  :safe t)

(defcustom banglejs-ble-serial-format
  '((:device . ?d)
    (:read-uuid . ?r)
    (:write-uuid . ?w)
    (:tcp-port . ?p)
    (:tcp-host . ?h)
    (:serial-device . ?s))
  "Alist of mappings between keywords in `banglejs-devices' and %-sequences used in
`banglejs-ble-serial-cmd'."
  :type '(alist :key-type character :value-type symbol)
  :safe t)

(defcustom banglejs-devices
  '(( :device nil
      :read-uuid "6e400003-b5a3-f393-e0a9-e50e24dcca9e"
      :write-uuid "6e400002-b5a3-f393-e0a9-e50e24dcca9e"
      :tcp-port 4002
      :serial-device "tmp/ttyBLE"))
  "List of Espruino devices.
Each element is a property list storing information used by
`banglejs-ble-serial-cmd-regexp' to connect a specific device to a serial port
or TCP server. You should change this variable to match your device(s)."
  :type '(repeat (plist :value-type (option string integer)))
  ;; safe to interpolate into banglejs-ble-serial-cmd as long as there is no pipe
  :safe (lambda (x)
          (not (map-some
                (lambda (_k v)
                  (and (stringp v) (string-match-p "|" v)))
                x))))

(defvar banglejs--ble-serial-process nil
  "Process started by `banglejs-ble-serial-start'.")

(defun banglejs-ble-serial--format-string (string &optional device)
  "Format STRING, using %-sequences defined in `banglejs-ble-serial-format', and
interpolating the data from DEVICE. By default, use `banglejs-devices'."
  (when string
    (format-spec string
                 (map-apply
                  (lambda (key val)
                    (let ((char (alist-get key banglejs-ble-serial-format)))
                      (when char (cons char val))))
                  (or device banglejs-devices)))))

(defun banglejs-ble-serial--process-running-p (&optional device)
  "Check if a (potentially external) ble-serial process is already running.
Look for a process whose executable name and invoking command line match the
regexps in `banglejs-ble-serial-cmd-regexp'. If that variable is nil, not check
is performed."
  (pcase-let* ((`(,cmd-regex ,arg-regex)
                (mapcar (lambda (reg)
                          (banglejs-ble-serial--format-string reg device))
                        banglejs-ble-serial-cmd-regexp)))
    (let (attributes)
      (and cmd-regex
           (seq-some (lambda (pid)
                       (and (setq attributes (process-attributes pid))
                            (string-match-p cmd-regex (alist-get 'comm attributes))
                            (or (not arg-regex)
                                (string-match-p arg-regex (alist-get 'args attributes)))))
                     (list-system-processes))))))

;;;###autoload
(defun banglejs-ble-serial-start (&optional device)
  "Start the ble-serial process if necessary.
Run the shell command `banglejs-ble-serial-cmd'. If the command contains
%-sequences, the values to be interpolated are taken from DEVICE.
The variable `banglejs-ble-serial-cmd-regexp' should match that command, to
detect whether the process is already running, potentially outside emacs.
The variable `banglejs-ble-serial-success-regexp' should be set to a regexp that
indicates the process is ready for `banglejs-term' to attempt to connect.
If DEVICE is nil, use `banglejs-devices'. Query the user if that list has
several elements."
  (interactive (list (if (> (length banglejs-devices) 1)
                         (completing-read "Connect to device: "
                                          (mapcar (lambda (d) (format "%s" d))
                                                  banglejs-devices))
                       (car banglejs-devices))))
  (let ((ble-buffer (get-buffer-create "*ble-serial*"))
        (cmd (banglejs-ble-serial--format-string banglejs-ble-serial-cmd device))
        (shell-command-dont-erase-buffer t))
    (unless (banglejs-ble-serial--process-running-p device)
      (message "Connecting…")
      (with-current-buffer ble-buffer
        (erase-buffer)
        (insert (format "%s\n-----\n" cmd))
        (async-shell-command cmd ble-buffer)
        (setq banglejs--ble-serial-process (get-buffer-process ble-buffer))
        ;; wait for the virtual device setup to succeed or fail
        (let ((idx 0))
          (while (and (accept-process-output banglejs--ble-serial-process)
                      (not (string-match-p banglejs-ble-serial-success-regexp
                                           (buffer-string) idx)))
            (setq idx (point)))))
      (unless (eq (process-status banglejs--ble-serial-process) 'run)
        (display-buffer ble-buffer)
        (error (format "Process %s failed" cmd))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; banglejs.el ends here
