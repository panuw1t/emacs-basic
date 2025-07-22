(setq dabbrev-case-replace nil)

;; (setq skeleton-pair t)
;; (electric-pair-mode 1)

(auto-insert-mode t)
(setq auto-insert-query nil)

(define-auto-insert "\\.kt\\'" 'my-kotlin-insert-template)

(defun my-spring-boot-make-package ()
	"Return a package name based on buffer or file."
	(when-let* ((root (project-current))
							(rel (file-relative-name	(buffer-file-name)	(project-root root)))
							(stripped (replace-regexp-in-string "^src/main/[^/]+/" "" rel))
							(dir (file-name-directory stripped)))
		(replace-regexp-in-string "/" "." (replace-regexp-in-string "/$" "" dir))))

(defun my-get-filename()
	"get filename without extension"
	(file-name-sans-extension (file-name-nondirectory (or (buffer-file-name) ""))))
	
(define-skeleton my-kotlin-insert-class
	"insert class kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"class " (my-get-filename) " {\n"
	> _ "\n}")

(define-skeleton my-kotlin-insert-data-class
	"insert data class kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"data class " (my-get-filename) "(\n"
	> _ "\n)")

(define-skeleton my-kotlin-insert-enum
	"insert enum class kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"enum class " (my-get-filename) " {\n"
	> _ "\n}")

(define-skeleton my-kotlin-insert-controller
	"insert controller class spring-boot kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"import org.springframework.web.bind.annotation.RestController\n"
	"import org.springframework.web.bind.annotation.RequestMapping\n\n"
	"@RestController\n"
	"@RequestMapping(\"\")" "\n"
	"class " (my-get-filename) " {\n"
	> _ "\n}")

(define-skeleton my-kotlin-insert-service
	"insert service class spring-boot kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"import org.springframework.stereotype.Service\n\n"
	"@Service\n"
	"class " (my-get-filename) " {\n"
	> _ "\n}")

(define-skeleton my-kotlin-insert-entity
	"insert entity class spring-boot kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"import jakarta.persistence.Entity\n"
	"import jakarta.persistence.GeneratedValue\n"
	"import jakarta.persistence.Id\n"
	"import jakarta.persistence.GenerationType\n"
	"import jakarta.persistence.Table\n\n"
	"@Entity\n"
	"@Table(name = \"\")\n"
	"class " (my-get-filename) "(\n"
	>	"@Id\n"
	> "@GeneratedValue(strategy = GenerationType.IDENTITY)\n"
  > "val id: Long = 0L,\n"
	> _ "\n)")

(define-skeleton my-kotlin-insert-repository
	"insert repository class spring-boot kotlin"
	nil
	(when-let ((package (my-spring-boot-make-package)))
		(concat "package " package "\n\n"))
	"import org.springframework.data.jpa.repository.JpaRepository\n\n"
	"interface " (my-get-filename) " : JpaRepository<" _ ", Long> {\n\n}")

(defun my-kotlin-insert-template ()
  "Prompt to insert a Kotlin class template."
  (interactive)
  (let* ((templates '(("data class" . my-kotlin-insert-data-class)
                      ("enum" . my-kotlin-insert-enum)
                      ("controller" . my-kotlin-insert-controller)
                      ("service" . my-kotlin-insert-service)
                      ("entity" . my-kotlin-insert-entity)
                      ("repository" . my-kotlin-insert-repository)
											("normal"      . my-kotlin-insert-class)))
				 (choice (completing-read "Insert Kotlin: " (mapcar #'car templates)))
				 (skeleton (cdr (assoc choice templates))))
		(when skeleton
			(funcall skeleton))))
