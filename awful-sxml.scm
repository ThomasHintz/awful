(define page-access-denied-message (make-parameter (lambda (path) '(h3 "Access denied."))))
(define web-repl-access-denied-message (make-parameter '(h3 "Access denied.")))
(define session-inspector-access-denied-message (make-parameter '(h3 "Access denied.")))
(define page-exception-message
  (make-parameter
   (lambda (exn)
     '(h3 "An error has occurred while processing your request."))))

(define (development-mode-actions)
  (print "Awful is running in development mode.")
  (debug-log (current-error-port))

  ;; Print the call chain, the error message and links to the
  ;; web-repl and session-inspector (if enabled)
  (page-exception-message
   (lambda (exn)
     `((pre ,(with-output-to-string
               (lambda ()
                 (print-call-chain)
                 (print-error-message exn))))
       (p "[" (a (@ (href ,(or (%web-repl-path) "/web-repl"))) "Web REPL") "]"
          ,(if (enable-session)
               `(" [" (a (@ (href ,(or (%session-inspector-path) "/session-inspector")))
                         "Session inspector") "]")
               "")))))

  ;; If web-repl has not been activated, activate it allowing access
  ;; to the localhost at least (`web-repl-access-control' can be
  ;; used to provide more permissive control)
  (unless (%web-repl-path)
          (let ((old-access-control (web-repl-access-control)))
            (web-repl-access-control
             (lambda ()
               (or (old-access-control)
                   (equal? (remote-address) "127.0.0.1")))))
          (enable-web-repl "/web-repl"))

  ;; If session-inspector has not been activated, and if
  ;; `enable-session' is #t, activate it allowing access to the
  ;; localhost at least (`session-inspector-access-control' can be
  ;; used to provide more permissive control)
  (when (and (enable-session) (not (%session-inspector-path)))
        (let ((old-access-control (session-inspector-access-control)))
          (session-inspector-access-control
           (lambda ()
             (or (old-access-control)
                 (equal? (remote-address) "127.0.0.1"))))
          (enable-session-inspector "/session-inspector")))

  ;; The reload page
  (define-reload-page))

(define (include-javascript . files)
  (map (lambda (file)
         `(script (@ (type "text/javascript") (src ,file))))
       files))

;;; Session-aware procedures for HTML code generation
(define (link url text . rest)
  (let ((pass-sid? (and (not (enable-session-cookie))
                        (sid)
                        (session-valid? (sid))
                        (not (get-keyword no-session: rest))))
        (arguments (or (get-keyword arguments: rest) '()))
        (separator (or (get-keyword separator: rest) ";&")))
    `(a
      (@ (href ,(if url
                    (string-append
                     url
                     (if (or pass-sid? (not (null? arguments)))
                         (string-append
                          "?"
                          (form-urlencode
                           (append arguments
                                   (if pass-sid?
                                       `((sid . ,(sid)))
                                       '()))
                           separator: separator))
                         ""))
                    "#"))
         ,@rest)
      ,text)))

(define (form contents . rest)
  (let ((pass-sid? (and (not (enable-session-cookie))
                        (sid)
                        (session-valid? (sid))
                        (not (get-keyword no-session: rest)))))
    `(form
      ,@rest
      ,(if pass-sid?
           `(input (@ (type "hidden") (name "sid") (value ,(sid))))
           '())
      ,contents)))

(define (run-resource proc path)
  (reset-per-request-parameters)
  (let ((handler
         (lambda (path proc)
           (let ((resp (proc path)))
             (if (procedure? resp)
                 (resp)
                 (let ((out (->string resp)))
                   (if (%error)
                       (send-response code: 500
                                      reason: "Internal server error"
                                      body: ((sxml->html) ((page-exception-message) (%error)))
                                      headers: '((content-type text/html)))
                       (if (%redirect) ;; redirection
                           (let ((new-uri (if (string? (%redirect))
                                              (uri-reference (%redirect))
                                              (%redirect))))
                             (with-headers `((location ,new-uri))
                                           (lambda ()
                                             (send-status 302 "Found"))))
                           (with-headers (append
                                          (or (awful-response-headers)
                                              `((content-type text/html)))
                                          (or (and-let* ((headers (awful-response-headers))
                                                         (content-length (alist-ref 'content-length headers)))
                                                (list (cons 'content-length content-length)))
                                              `((content-length ,(string-length out)))))
                                         (lambda ()
                                           (write-logged-response)
                                           (unless (eq? 'HEAD (request-method (current-request)))
                                             (display out (response-port (current-response))))))))))))))
    (call/cc (lambda (continue)
               (for-each (lambda (hook)
                           ((cdr hook) path
                                       (lambda ()
                                         (handler path proc)
                                         (continue #f))))
                         *request-handler-hooks*)
               (handler path proc)))
    ;; The value for %path-procedure-result is determined at path
    ;; matching time, before run-resource is called.  If it was reset
    ;; by reset-per-request-parameters (which is called right at the
    ;; beginning of run-resource), its value would be reset.  So we
    ;; reset it here, after the page handler used its value and
    ;; has finished.
    (%path-procedure-result not-set)))

(define (maybe-literal-javascript js)
  (if (literal-script/style?)
      `(literal ,js)
      js))

(define (include-page-javascript ajax? no-javascript-compression)
  (if ajax?
      `(script (@ (type "text/javascript"))
                ,(maybe-literal-javascript
                 (maybe-compress-javascript
                  (++ "$(document).ready(function(){"
                      (page-javascript) "});")
                  no-javascript-compression)))
      (if (string-null? (page-javascript))
          ""
          `(script (@ (type "text/javascript"))
                   ,(maybe-literal-javascript
                     (maybe-compress-javascript
                      (page-javascript)
                      no-javascript-compression))))))

(define (include-page-css)
  (if (%page-css)
      `(style ,(if (literal-script/style?)
                   `(literal ,(%page-css))
                   (%page-css)))
      ""))

(define-inline (apply-page-template contents css title doctype ajax? use-ajax headers
                                    charset no-javascript-compression)
  ((sxml->html)
   (parameterize ((generate-sxml? #t))
                 ((page-template)
                  contents
                  css: (or css (page-css))
                  title: (or (%page-title) title)
                  doctype: (or doctype (page-doctype))
                  headers: `(,(include-page-css)
                             ,(if ajax?
                                  `(script (@ (type "text/javascript")
                                              (src ,(if (string? use-ajax)
                                                        use-ajax
                                                        (ajax-library)))))
                                  "")
                             ,(or headers '())
                             ,(if (eq? (javascript-position) 'top)
                                  (include-page-javascript ajax? no-javascript-compression)
                                  '()))
                  charset: (or charset (page-charset))
                  literal-style?: (literal-script/style?)))))

(define-inline (render-exception exn)
  (%error exn)
  (debug (with-output-to-string
           (lambda ()
             (print-call-chain)
             (print-error-message exn))))
  ((page-exception-message) exn))

(define-inline (render-page contents path given-path no-javascript-compression ajax?)
  (let ((resp
         (cond ((regexp? path)
                (contents given-path))
               ((not (not-set? (%path-procedure-result)))
                (let ((result (%path-procedure-result)))
                  (apply contents result)))
               (else (contents)))))
    (if (procedure? resp)
        ;; eval resp here, where all
        ;; parameters' values are set
        (let ((out (resp))) (lambda () out))
        `(,resp
          ,(if (eq? (javascript-position) 'bottom)
               (include-page-javascript ajax? no-javascript-compression)
               '())))))

(define (define-page path contents #!key css title doctype headers charset no-ajax
                     no-template no-session no-db vhost-root-path no-javascript-compression
                     use-ajax (method '(GET HEAD))
                     use-session) ;; for define-session-page
  (##sys#check-closure contents 'define-page)
  (let ((path (page-path path)))
    (add-resource!
     path
     (or vhost-root-path (root-path))
     (lambda (#!optional given-path)
       (sid (get-sid use-session))
       (when (and (db-credentials) (db-enabled?) (not no-db))
         (db-connection ((db-connect) (db-credentials))))
       (page-javascript "")
       (%page-css #f)
       (awful-refresh-session!)
       (let ((out
              (if (use-session? use-session no-session)
                  (if ((page-access-control) (or given-path path))
                      (begin
                        (maybe-create/refresh-session! use-session)
                        (let* ((ajax? (use-ajax? use-ajax no-ajax))
                               (contents
                                (handle-exceptions exn
                                  (render-exception exn)
                                  (render-page contents path given-path no-javascript-compression ajax?))))
                          (if (%redirect)
                              #f ;; no need to do anything.  Let `run-resource' perform the redirection
                              (if (procedure? contents)
                                  contents
                                  (if no-template
                                      ((sxml->html) contents)
                                      (apply-page-template contents css title doctype ajax? use-ajax headers charset
                                                           no-javascript-compression))))))
                      ((page-template) ((page-access-denied-message) (or given-path path))))
                  (redirect-to-login-page (or given-path path)))))
         (when (and (db-connection) (db-enabled?) (not no-db)) ((db-disconnect) (db-connection)))
         out))
     method))
  path)



(define (ajax path id event proc #!key (action 'html) (method 'POST) (arguments '())
              target success no-session no-db no-page-javascript vhost-root-path
              live on content-type prelude update-targets (cache 'not-set) error-handler)
  (when (and on live)
    (error 'ajax "`live' and `on' cannot be used together."))
  (let ((path (page-path path (ajax-namespace))))
    (add-resource! path
                   (or vhost-root-path (root-path))
                   (lambda (#!optional given-path)
                     (sid (get-sid 'force))
                     (when update-targets
                       (awful-response-headers '((content-type "application/json"))))
                     (if (or (not (enable-session))
                             no-session
                             (and (enable-session) (session-valid? (sid))))
                         (if ((page-access-control) path)
                             (begin
                               (when (and (db-credentials) (db-enabled?) (not no-db))
                                 (db-connection ((db-connect) (db-credentials))))
                               (awful-refresh-session!)
                               (let* ((out (if update-targets
                                               (with-output-to-string
                                                 (lambda ()
                                                   (json-write
                                                    (list->vector
                                                     (map (lambda (id/content)
                                                            (cons (car id/content) ((sxml->html) (cdr id/content))))
                                                          (proc))))))
                                               ((sxml->html) (proc)))))
                                 (when (and (db-credentials) (db-enabled?) (not no-db))
                                   ((db-disconnect) (db-connection)))
                                 out))
                             ((page-access-denied-message) path))
                         (ajax-invalid-session-message)))
                   method)
    (let* ((arguments (if (and (sid) (session-valid? (sid)))
                          (cons `(sid . ,(++ "'" (sid) "'")) arguments)
                          arguments))
           (js-code
            (++ (if (and id event)
                    (let ((events (concat (if (list? event) event (list event)) " "))
                          (binder (if live "live" (if on "on" "bind"))))
                      (++ (if on
                              (if (string? on)
                                  (sprintf "$('~a')." on) ;; start delegation from `on'
                                  "$(document).")
                              (sprintf "$('~a')." (->jquery-selector id)))
                          binder "('" events "',"))
                    "")
                (if (and on (not (boolean? on)))
                    (sprintf "'~a'," (->jquery-selector on))
                    "")
                (++ "function(event){"
                    (or prelude "")
                    "$.ajax({type:'" (->string method) "',"
                    "url:'" path "',"
                    (if content-type
                        (conc "contentType: '" content-type "',")
                        "")
                    "success:function(response){"
                    (or success
                        (cond (update-targets
                               "$.each(response, function(id, html) { $('#' + id).html(html);});")
                              (target
                               (++ "$('#" target "')." (->string action) "(response);"))
                              (else "return;")))
                    "},"
                    (if update-targets
                        "dataType: 'json',"
                        "")
                    (if (eq? cache 'not-set)
                        ""
                        (if cache
                            "cache:true,"
                            "cache:false,"))
                    (if error-handler
                        (++ "error:" error-handler ",")
                        "")
                    (++ "data:{"
                        (string-intersperse
                         (map (lambda (var/val)
                                (conc  "'" (car var/val) "':" (cdr var/val)))
                              arguments)
                         ",") "}")
                    "})}")
                (if (and id event)
                    ");\n"
                    ""))))
      (unless no-page-javascript (add-javascript js-code))
      js-code)))


(define (periodical-ajax path interval proc #!key target (action 'html) (method 'POST)
                         (arguments '()) success no-session no-db vhost-root-path live on
                         content-type prelude update-targets cache error-handler)
  (add-javascript
   (++ "setInterval("
       (ajax path #f #f proc
             target: target
             action: action
             method: method
             arguments: arguments
             success: success
             no-session: no-session
             no-db: no-db
             vhost-root-path: vhost-root-path
             live: live
             on: on
             content-type: content-type
             prelude: prelude
             update-targets: update-targets
             error-handler: error-handler
             cache: cache
             no-page-javascript: #t)
       ", " (->string interval) ");\n")))


(define (ajax-link path id text proc #!key target (action 'html) (method 'POST) (arguments '())
                   success no-session no-db (event 'click) vhost-root-path live on class
                   hreflang type rel rev charset coords shape accesskey tabindex a-target
                   content-type prelude update-targets error-handler cache)
  (ajax path id event proc
        target: target
        action: action
        method: method
        arguments: arguments
        success: success
        no-session: no-session
        vhost-root-path: vhost-root-path
        live: live
        on: on
        content-type: content-type
        prelude: prelude
        update-targets: update-targets
        error-handler: error-handler
        cache: cache
        no-db: no-db)
  `(a (@ (href "#")
         (id ,id)
         (class ,class)
         (hreflang ,hreflang)
         (type ,type)
         (rel ,rel)
         (rev ,rev)
         (charset ,charset)
         (coords, coords)
         (shape ,shape)
         (accesskey ,accesskey)
         (tabindex ,tabindex)
         (target ,a-target))
      ,text))




;;; Login form
(define (login-form #!key (user-label "User: ")
                          (password-label "Password: ")
                          (submit-label "Submit")
                          (trampoline-path "/login-trampoline")
                          (refill-user #t))
  (let ((attempted-path ($ 'attempted-path))
        (user ($ 'user)))
    `(form (@ (action ,trampoline-path) (method "post"))
           ,(if attempted-path
                `(input (@ (type "hidden") (name "attempted-path") (value ,attempted-path)))
                '())
           (span (@ (id "user-container"))
                 (label (@ (id "user-label") (for "user")) ,user-label)
                 (input (@ (type "text") (id "user") (name "user") (value ,(and refill-user user)))))
           (span (@ (id "password-container"))
                 (label (@ (id "password-label") (for "password")) ,password-label)
                 (input (@ (type "password") (id "password") (name "password"))))
           (input (@ (type "submit") (id "login-submit") (value ,submit-label))))))

;;; Login trampoline (for redirection)
(define (define-login-trampoline path #!key vhost-root-path hook)
  (define-page path
    (lambda ()
      (let* ((user ($ 'user))
             (password ($ 'password))
             (attempted-path ($ 'attempted-path))
             (password-valid? ((valid-password?) user password))
             (new-sid (and password-valid? (session-create))))
        (sid new-sid)
        (when (enable-session-cookie)
              ((session-cookie-setter) new-sid))
        (when hook (hook user))
        (parameterize ((generate-sxml? #t))
                      (html-page
         ""
         headers: `(meta (@ (http-equiv "refresh")
                            (content ,(++ "0;url="
                                          (if new-sid
                                              (++ (or attempted-path (main-page-path))
                                                  "?user=" user
                                                  (if (enable-session-cookie)
                                                      ""
                                                      (++ "&sid=" new-sid)))
                                              (++ (login-page-path) "?reason=invalid-password&user=" user))))))))))
    method: 'POST
    vhost-root-path: vhost-root-path
    no-session: #t
    no-template: #t))

(define (enable-web-repl path #!key css (title "Awful Web REPL") headers)
  (unless (development-mode?) (%web-repl-path path))
  (define (maybe-literal . content)
    `(literal ,@content))

  (define (fancy-editor-js)
    (if (enable-web-repl-fancy-editor)
        `(script (@ (type: "text/javascript"))
                 (literal ,(string-append "
  function addClass(element, className) {
    if (!editor.win.hasClass(element, className)) {
      element.className = ((element.className.split(' ')).concat([className])).join(' ');}}

  function removeClass(element, className) {
    if (editor.win.hasClass(element, className)) {
      var classes = element.className.split(' ');
      for (var i = classes.length - 1 ; i >= 0; i--) {
        if (classes[i] === className) {
            classes.splice(i, 1)}}
      element.className = classes.join(' ');}}

  var textarea = document.getElementById('prompt');
  var editor = new CodeMirror(CodeMirror.replace(textarea), {
    height: '250px',
    width: '600px',
    content: textarea.value,
    parserfile: ['" (web-repl-fancy-editor-base-uri) "/tokenizescheme.js',
                 '" (web-repl-fancy-editor-base-uri) "/parsescheme.js'],
    stylesheet:  '" (web-repl-fancy-editor-base-uri) "/schemecolors.css',
    autoMatchParens: true,
    path: '" (web-repl-fancy-editor-base-uri) "/',
    disableSpellcheck: true,
    markParen: function(span, good) {addClass(span, good ? 'good-matching-paren' : 'bad-matching-paren');},
    unmarkParen: function(span) {removeClass(span, 'good-matching-paren'); removeClass(span, 'bad-matching-paren');}
  });")))
        '()))

  (define (web-repl-css)
    (let ((builtin-css
           `(style (@ (type "text/css"))
              ,(string-append
                "h1 { font-size: 18pt; background-color: #898E79; width: 590px; color: white; padding: 5px;}"
                "h2 { font-size: 14pt; background-color: #898E79; width: 590px; color: white; padding: 5px;}"
                "ul#button-bar { margin-left: 0; padding-left: 0; }"
                "#button-bar li {display: inline; list-style-type: none; padding-right: 10px; }"
                (if (enable-web-repl-fancy-editor)
                    "div.border { border: 1px solid black; width: 600px;}"
                    "#prompt { width: 600px; }")
                "#result { border: 1px solid #333; padding: 5px; width: 590px; }"))))
      (if headers
          (append (or css builtin-css) headers)
          (or css builtin-css))))

  (define (web-eval)
    `(pre ,(with-output-to-string
             (lambda ()
               (pp (handle-exceptions exn
                     (begin
                       (print-error-message exn)
                       (print-call-chain))
                     (eval `(begin
                              ,@(with-input-from-string ($ 'code "")
                                  read-file)))))))))

  (define-page path
    (lambda ()
      (if ((web-repl-access-control))
          (begin
            (page-javascript
             (string-append "$('#clear').click(function(){"
                            (if (enable-web-repl-fancy-editor)
                                "editor.setCode('');"
                                "$('#prompt').val('');")
                            "});"))

            (ajax (string-append path "-eval") 'eval 'click web-eval
                  target: "result"
                  arguments: `((code . ,(if (enable-web-repl-fancy-editor)
                                            "editor.getCode()"
                                            "$('#prompt').val()"))))

            (when (enable-web-repl-fancy-editor)
              (ajax (string-append path "-eval") 'eval-region 'click web-eval
                    target: "result"
                    arguments: `((code . "editor.selection()"))))

            `((h1 ,title)
              (h2 "Input area")
              ,(let ((prompt `(textarea (@ (id "prompt") (name "prompt") (rows "10") (cols "90")))))
                 (if (enable-web-repl-fancy-editor)
                     `(div (@ (class "border")) ,prompt)
                     prompt))
              (ul (@ (id "button-bar"))
                  ,(map (lambda (item)
                          `(li (button (@ (id ,(car item))) ,(cdr item))))
                        (append '(("eval"  . "Eval"))
                                (if (enable-web-repl-fancy-editor)
                                    '(("eval-region" . "Eval region"))
                                    '())
                                '(("clear" . "Clear")))))
              (h2 "Output area")
              (div (@ (id "result")))
              ,(fancy-editor-js)))
          (parameterize ((generate-sxml? #t))
            (web-repl-access-denied-message))))
    headers: (append
              (if (enable-web-repl-fancy-editor)
                  (include-javascript (make-pathname (web-repl-fancy-editor-base-uri) "codemirror.js")
                                      (make-pathname (web-repl-fancy-editor-base-uri) "mirrorframe.js"))
                  '())
              (list (web-repl-css)))
    use-ajax: #t
    title: title
    css: css))

(define (enable-session-inspector path #!key css (title "Awful session inspector") headers)

  (unless (development-mode?)
    (%session-inspector-path path))

  (define builtin-css
    `(style (@ (type "text/css"))
       "h1 { font-size: 16pt; background-color: #898E79; width: 590px; color: white; padding: 5px;}\n"
       ".session-inspector-value { margin: 2px;}\n"
       ".session-inspector-var { margin: 0px; }\n"
       "#session-inspector-table { margin: 0px; width: 600px;}\n"
       "#session-inspector-table tr td, th { padding-left: 10px; border: 1px solid #333; vertical-align: middle; }\n"))

  (define-page path
    (lambda ()
      (parameterize ((enable-session #t))
        (if ((session-inspector-access-control))
            (let ((bindings (session-bindings (sid))))
              `((h1 title)
                ,(if (null? bindings)
                     `(p "Session for sid " ,(sid) " is empty")
                     `((p "Session for " ,(sid))
                       (table (@ (id "session-inspector-table"))
                              (tr (th "Variables")
                                  (th "Values"))
                              ,@(map (lambda (binding)
                                       (let ((var (car binding))
                                             (val (with-output-to-string
                                                    (lambda ()
                                                      (pp (cdr binding))))))
                                         `(tr (td (span (@ (class "session-inspector-var"))
                                                        ,var))
                                              (td (pre (@ (class "session-inspector-value"))
                                                       ,val)))))
                                     bindings))))))
            (session-inspector-access-denied-message))))
    headers: (if headers
                 (append (or css builtin-css) headers)
                 (or css builtin-css))
    title: title
    css: css))
