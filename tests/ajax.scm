#!/usr/bin/csi -script

(use posix awful html-tags)

(enable-ajax #t)

(define-page (main-page-path)
  (lambda ()
    
    (define (show-secs) (->string (current-seconds)))
    
    (ajax "/single" 'single 'click show-secs target: "single-target")
    (ajax "/bind" 'bind '(click dblclick) show-secs target: "bind-target")
    (ajax "/live" 'live '(click dblclick)
          (lambda ()
            (++ (show-secs)
                (<a> href: "#" id: "after-life" "Click me again")))
          target: "live-target")
    (ajax "/after-life" 'after-life 'click show-secs target: "after-life-target" live: #t)

    
    (++ (<h2> "Single event")
        (<a> href: "#" id: "single" "Click me")
        (<div> id: "single-target")

        (<h2> "Multiple events (bind)")
        (<a> href: "#" id: "bind" "Click me")
        (<div> id: "bind-target")
        
        (<h2> "Live events")
        (<a> href: "#" id: "live" "Click me")
        (<div> id: "live-target")
        (<div> id: "after-life-target")
        )))
