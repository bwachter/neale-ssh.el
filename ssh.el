;;
;; ssh
;;
(defvar ssh/default-host "fsf.org"
  "Default if you just hit enter for hostname")

(defvar ssh/frequent-hosts '("host1.fsf.org")
  "List of hosts to add to completion options")

(defun ssh/remove-known-host (arg)
  "Prompt to select and remove a host from ssh known_hosts file.

With prefix ARG, prompt for an alternative known_hosts file path."
  (interactive "P")
  (let* ((known-hosts-file (if arg
                               (read-file-name "Known hosts file: " "~/.ssh/" nil t)
                             (or (getenv "KNOWN_HOSTS") (expand-file-name "~/.ssh/known_hosts"))))
         (hosts '()))
    (unless (file-readable-p known-hosts-file)
      (user-error "Known hosts file not readable: %s" known-hosts-file))
    ;; Parse known_hosts file, extract first field from each line (hostnames), stripping commas and duplicates
    (with-temp-buffer
      (insert-file-contents known-hosts-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
          (when (string-match "^\\([^ ]+\\)" line)
            (let* ((hostfield (match-string 1 line))
                   (host (car (split-string hostfield ","))))
              (push host hosts))))
        (forward-line 1)))
    (setq hosts (delete-dups (reverse hosts)))
    (if (null hosts)
        (user-error "No hosts found in %s" known-hosts-file)
      (let* ((host-to-remove (completing-read "Select host to remove: " hosts nil t))
             (cmd (format "ssh-keygen -f %s -R %s" (shell-quote-argument known-hosts-file) (shell-quote-argument host-to-remove))))
        (shell-command cmd)
        (message "Removed host %s from %s" host-to-remove known-hosts-file)))))

(defun ssh/known-hosts ()
  "Return a list of hosts for completion"
  (with-temp-buffer
    (insert-file-contents-literally "~/.ssh/known_hosts")
    (let ((ssh-hosts-list) '())
      (while (not (eobp))
	(add-to-list 'ssh-hosts-list (buffer-substring (point) (- (search-forward-regexp "[, ]") 1)))
	(forward-line))
      ssh-hosts-list)))

(defun ssh/config-hosts ()
  "Return list of SSH hosts from user's .ssh/config, excluding wildcards."
  (let ((config-file (expand-file-name "~/.ssh/config"))
        hosts)
    (when (file-readable-p config-file)
      (with-temp-buffer
        (insert-file-contents config-file)
        (goto-char (point-min))
        (while (re-search-forward "^Host[ \t]+\\(.*\\)$" nil t)
          (let* ((line (match-string 1))
                 (split-hosts (split-string line "[ \t]+" t)))
            (dolist (h split-hosts)
              (unless (string-match-p "[*?\\[\\]]" h)
                (push h hosts)))))))
    (delete-dups hosts)))

(setq ssh/host-history '())
(defun ssh (prefix remote)
  (interactive
   (list
    current-prefix-arg
    (ssh/read-host (format "Remote host (default %s): " ssh/default-host) current-prefix-arg)))
  (if (string= remote "")
      (setq remote ssh/default-host))
  (let ((name (generate-new-buffer-name (format "*%s*" remote)))
	(default-directory "/tmp")
	(explicit-shell-file-name "ssh")
	(explicit-ssh-args (list remote)))
    (shell name)
    ;; Doing it this way is goofy, but whatevs.
    (with-current-buffer name
      ;; HP iLO needs a carriage return instead of newline.
      (if (string-match "\.ilo$" remote)
	  (setq-local comint-input-sender 'ssh/comint-cr-send))
      (setq-local dabbrev-abbrev-char-regexp "\\sw\\|\\s_\\|[-._,]"))))

(defun ssh/eat (prefix)
  "Open SSH connection with host completion inside an EAT terminal.

When called with prefix will prompt for user/host, otherwise only host, with
completion from known hosts and ssh config."
  (interactive "P")
  (let* ((host (ssh/read-host (format "Remote host (default %s): " ssh/default-host) prefix))
         (title-cmd (format "echo -ne \"\\033]0;ssh:%s\\007\"" host))
         (cmd (format "%s;TERM=xterm-256color ssh %s" title-cmd host)))
    (eat cmd)))

(defun mosh (prefix)
  "Open mosh connection with host completion inside an EAT terminal.

When called with prefix will prompt for user/host, otherwise only host, with
completion from known hosts and ssh config."
  (interactive "P")
  (let* ((host (ssh/read-host (format "Mosh host (default %s): " ssh/default-host) prefix))
         (title-cmd (format "echo -ne \"\\033]0;mosh:%s\\007\"" host))
         (cmd (format "%s;TERM=xterm-256color mosh %s" title-cmd host)))
    (eat cmd)))

(defun ssh/read-host (prompt ask-user)
  "Read a host with optional username

When called with `ask-user' set to `t' this will ask for a username, followed by
a host. With `ask-user' set to 'nil' only prompt for the host.

It's recommended for calling functions to support a prefix argument, and pass
the prefix as `ask-user'"
  (interactive)
  (let* ((hosts (append ssh/frequent-hosts (ssh/config-hosts) (ssh/known-hosts)))
         (user (when ask-user (read-string "Username: ")))
         (host (completing-read prompt hosts nil nil "" 'ssh/host-history))
         (host (if (string= host "") ssh/default-host host)))
    (if user (format "%s@%s" user host) host)))

(defun ssh/tramp (prefix)
  "Open a Tramp SSH connection using

This might be a more friendly interface than the standard file open prompt for
tramp/ssh"
  (interactive "P")
  (let* ((host (ssh/read-host (format "Remote host (default %s): " ssh/default-host) prefix))
         (tramp-path (format "/ssh:%s:" host)))
    (find-file tramp-path)))

;;
;; Kill old buffers; not sure this is a fantastic idea
;;
(defun ssh/shell-kill-buffer-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (kill-buffer (process-buffer process))))

(defun ssh/kill-process-buffer-on-exit ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'ssh/shell-kill-buffer-sentinel))
(add-hook 'comint-exec-hook 'ssh/kill-process-buffer-on-exit)

(defun ssh/comint-cr-send (proc string)
  "Send a comint string terminated with carriage return

Some machines (HP iLO) need this, because reasons."
  (let ((send-string
         (if comint-input-sender-no-newline
             string
           (concat string "\r"))))
    (comint-send-string proc send-string))
  (if (and comint-input-sender-no-newline
	   (not (string-equal string "")))
      (process-send-eof)))

(defun mmb-proceed ()
  (interactive)
  (with-current-buffer "*mmb*"
    (start-file-process "mmb" (current-buffer) "~/work/bin/mmb" "-a" ircname)))

(defun mmb (who)
  (interactive "MMerge who? ")
  (with-current-buffer (get-buffer-create "*mmb*")
    (setq-local ircname who)
    (display-buffer (current-buffer))
    (erase-buffer)
    (start-file-process "mmb" (current-buffer) "~/work/bin/mmb" ircname)
    (local-set-key (kbd "C-c C-c") 'mmb-proceed)))

(provide 'ssh)
