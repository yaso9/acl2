(in-package "X86ISA")

(include-book "../machine/x86" :ttags :all)
(include-book "std/io/top" :dir :system)

(defstobj inst-test-record
          (itr-reg-vals :type (array (unsigned-byte 64) (3))
                        :resizable nil
                        :initially 0)
          (itr-value :type (unsigned-byte 64)
                     :initially 0)
          (itr-flags :type (unsigned-byte 64)
                     :initially 0)
          :non-executable t)

(defstobj inst-reg-record
          (irr-inst-buf :type (array (unsigned-byte 8) (15))
                        :resizable nil
                        :initially 0)
          (irr-regs :type (array (unsigned-byte 8) (3))
                    :resizable nil
                    :initially 0)
          (irr-value-reg :type (unsigned-byte 8)
                         :initially 0)
          (irr-flags-mask :type (unsigned-byte 64)
                          :initially 0)
          (irr-itrs :type (array inst-test-record (0))
                    :resizable t)
          :non-executable t)

(defstobj dandelion-record
          ;; We include this len field to allow for geometric resizing as we
          ;; read the file
          (dr-len :type unsigned-byte :initially 0)
          (dr-irrs :type (array inst-reg-record (1))
                   :resizable t))

(define read-itr-reg-vals (channel (i :type (integer 0 2)) inst-test-record state)
  :returns (mv (eofp booleanp)
               inst-test-record
               state)
  (b* (((mv reg-val state) (acl2::read-bytes$ channel :bytes 8 :end :little))
       ((unless (integerp reg-val) (mv t inst-test-record state)))
       (inst-test-record (update-itr-reg-valsi i reg-val inst-test-record))

       ((when (equal i 2)) (mv nil inst-test-record state)))
      (read-itr-reg-vals channel (1+ i) inst-test-record state)))

(define read-inst-test-record (channel inst-test-record state)
  :returns (mv inst-test-record state)
  (b* (((mv eofp inst-test-record state) (read-itr-reg-vals 0 inst-test-record state))
       ((when eofp) (prog2$ (er hard? 'read-inst-test-records "Unexpected EOF")
                            (mv inst-test-record state)))

       ((mv value state) (acl2::read-bytes$ channel :bytes 8 :end :little))
       ((unless (integerp value)) (prog2$ (er hard? 'read-inst-test-records "Unexpected EOF")
                                          (mv inst-test-record state)))
       (inst-test-record (update-itr-value value inst-test-record))

       ((mv flags state) (acl2::read-bytes$ channel :bytes 8 :end :little))
       ((unless (integerp flags)) (prog2$ (er hard? 'read-inst-test-records "Unexpected EOF")
                                          (mv inst-test-record state)))
       (inst-test-record (update-itr-flags flags inst-test-record)))
      (mv inst-test-record state)))

(define read-inst-test-records (channel (i :type unsigned-byte) inst-reg-record state)
  :returns (mv inst-reg-record state)
  (b* (((when (equal i (length-irr-itrs inst-reg-record)))
        (mv inst-reg-record state))

       ((mv inst-reg-record state)
        (stobj-let (inst-test-record (irr-itrsi i inst-reg-record))
                   (inst-test-record state)
                   (read-inst-test-record channel inst-test-record state)
                   (mv inst-reg-record state)))
       (read-inst-test-records channel (1+ i) inst-reg-record state))))

(define read-irr-inst-buf ((i :type (integer 0 14))
                           channel inst-reg-record state)
  :returns ((eofp booleanp)
            inst-reg-record
            state)
  (b* (((mv byt state) (read-byte$ channel state))
       ((unless byt) (mv t inst-reg-record state))
       (inst-reg-record (update-irr-inst-buf i inst-reg-record))
 
       ((when (equal i 15)) (mv nil inst-reg-record state)))
      (read-irr-inst-buf (1+ i) channel inst-reg-record state)))

(define read-irr-regs ((i :type (integer 0 2))
                       channel inst-reg-record state)
  :returns ((eofp booleanp)
            inst-reg-record
            state)
  (b* (((mv byt state) (read-byte$ channel state))
       ((unless byt) (mv t inst-reg-record state))
       (inst-reg-record (update-irr-regs i inst-reg-record))
 
       ((when (equal i 2)) (mv nil inst-reg-record state)))
      (read-irr-regs (1+ i) channel inst-reg-record state)))

(define read-inst-reg-record (channel inst-reg-record state)
  :returns ((eofp booleanp)
            inst-reg-record
            state)
  (b* (;; I read the first byte separately to check for EOF
       ;; If there is an EOF, it should be at the first byte. If we get
       ;; one later, something is broken somewhere.
       ((mv byt state) (read-byte$ channel state))
       ((unless byt) (mv t inst-reg-record state))
       (inst-reg-record (update-irr-inst-buf 0 inst-reg-record))

       ;; Read rest of the inst-buf
       ((mv eofp inst-reg-record state) (read-irr-inst-buf 1 channel inst-reg-record state))
       ((when eofp) (prog2$ (er hard? 'read-inst-reg-record "Unexpected EOF")
                            (mv t inst-reg-record state)))

       ((mv eofp inst-reg-record state) (read-irr-regs 0 channel inst-reg-record state))
       ((when eofp) (prog2$ (er hard? 'read-inst-reg-record "Unexpected EOF")
                            (mv t inst-reg-record state)))

       ((mv value-reg state) (read-byte$ channel state))
       ((unless value-reg) (prog2$ (er hard? 'read-inst-reg-record "Unexpected EOF")
                                   (mv t inst-reg-record state)))
       (inst-reg-record (update-irr-value-reg value-reg inst-reg-record))

       ((mv flags-mask state) (acl2::read-bytes$ channel :bytes 8 :end :little))
       ((unless (integerp flags-mask)) (prog2$ (er hard? 'read-inst-reg-record "Unexpected EOF")
                                              (mv t inst-reg-record state)))
       (inst-reg-record (update-irr-flags-mask flags-mask inst-reg-record))

       ((mv n-itrs state) (acl2::read-bytes$ channel :bytes 8 :end :little))
       ((unless (integerp n-itrs)) (prog2$ (er hard? 'read-inst-reg-record "Unexpected EOF")
                                              (mv t inst-reg-record state)))
       (inst-reg-record (irr-itrs-resize n-itrs inst-reg-record))

       ((mv inst-reg-record state) (read-inst-test-records channel 0 inst-reg-record state)))
      (mv inst-reg-record state)))

(define read-dandelion-record (channel dandelion-record state)
  :returns (dandelion-record state)
  (b* ((capacity (dr-irrs-length dandelion-record))
       (init-len (dr-len dandelion-record))
       (dandelion-record (if (equal capacity init-len)
                           ;; We need to grow
                           (resize-dr-irrs (* 2 capacity) dandelion-record)
                           dandelion-record))

       ((mv eofp dandelion-record state)
        (stobj-let ((inst-reg-record (dr-irrsi init-len dandelion-record)))
                   (eofp inst-reg-record state)
                   (read-inst-reg-record inst-reg-record)
                   (mv eofp dandelion-record state)))

       ((when eofp)
        (b* ((dandelion-record (resize-dr-irrs init-len dandelion-record)))
            (mv dandelion-record state)))

       (dandelion-record (update-dr-len (1+ init-len) dandelion-record)))
      (read-dandelion-record channel dandelion-record state)))
