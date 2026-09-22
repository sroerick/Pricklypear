;; libs/rb/lib.pp — root beer recipe DSL for Pricklypear
;; Port of rb (OCaml) into Nopales. The recipe IS the program.
;;
;; A recipe is a Nopales expression that evaluates into an alist (list of
;; pairs). Calc functions operate on those alists. Persistence to the tree
;; is a separate concern (rb/save! → create 'recipe).

;; ============================================================
;; ALIST HELPERS
;; ============================================================

;; assoc returns (key . value) or () if missing; rb/get unwraps it.
(define (rb/get key alist)
  "Look up key in an alist; return its value or ()."
  (let ((pair (assoc key alist)))
    (if (null? pair) () (cdr pair))))

(define (rb/has? key alist)
  "True when key is present in the alist."
  (not (null? (assoc key alist))))

(define (rb/set key val alist)
  "Prepend (key . val) onto alist."
  (cons (cons key val) alist))

(define (rb/append a b)
  "Concatenate two lists."
  (if (null? a) b (cons (car a) (rb/append (cdr a) b))))

(define (rb/map fn xs)
  "Map fn over a cons-built list."
  (if (null? xs) (list)
    (cons (fn (car xs)) (rb/map fn (cdr xs)))))

(define (rb/filter pred xs)
  "Keep elements of xs satisfying pred."
  (if (null? xs) (list)
    (if (pred (car xs))
        (cons (car xs) (rb/filter pred (cdr xs)))
      (rb/filter pred (cdr xs)))))

(define (rb/sum fn xs)
  "Sum fn applied to each element, as a float."
  (if (null? xs) 0.0 (+ (fn (car xs)) (rb/sum fn (cdr xs)))))

;; count that works on cons-built lists (not just VLists)
(define (rb/count xs)
  "Number of elements (works on cons-built lists)."
  (if (null? xs) 0 (+ 1 (rb/count (cdr xs)))))

;; ============================================================
;; UNITS — dimension table and conversion
;; Base units: Volume→L, Mass→Lb, Count→Each, Time→Min
;; ============================================================

(define rb/unit-table
  (list
    (cons "L"     (cons "volume" 1.0))
    (cons "mL"    (cons "volume" 0.001))
    (cons "gal"   (cons "volume" 3.78541))
    (cons "qt"    (cons "volume" 0.946353))
    (cons "fl_oz" (cons "volume" 0.0295735))
    (cons "cup"   (cons "volume" 0.236588))
    (cons "tbsp"  (cons "volume" 0.0147868))
    (cons "tsp"   (cons "volume" 0.00492892))
    (cons "lb"    (cons "mass" 1.0))
    (cons "oz"    (cons "mass" 0.0625))
    (cons "kg"    (cons "mass" 2.20462))
    (cons "g"     (cons "mass" 0.00220462))
    (cons "each"  (cons "count" 1.0))
    (cons "batch" (cons "count" 1.0))
    (cons "min"   (cons "time" 1.0))
    (cons "hr"    (cons "time" 60.0))
    (cons "day"   (cons "time" 1440.0))))

(define (rb/unit-dim u)   "Dimension of a unit string (volume/mass/count/time)." (car (rb/get u rb/unit-table)))
(define (rb/unit-factor u) "Factor of a unit relative to its base unit." (cdr (rb/get u rb/unit-table)))

;; quantity: (value . "unit")
(define (qty val unit) "Build a quantity (value . unit)." (cons val unit))
(define (qty-val q) "Value part of a quantity." (car q))
(define (qty-unit q) "Unit string of a quantity." (cdr q))

(define (rb/to-base q)
  "Convert a quantity to its base unit."
  (* (qty-val q) (rb/unit-factor (qty-unit q))))

(define (rb/compatible? q1 q2)
  "True when two quantities share a dimension."
  (string-eq (rb/unit-dim (qty-unit q1)) (rb/unit-dim (qty-unit q2))))

(define (rb/convert q target-unit)
  "Convert a quantity to a target unit within its dimension."
  (if (not (string-eq (rb/unit-dim (qty-unit q)) (rb/unit-dim target-unit)))
      q
    (let ((base (rb/to-base q)))
      (qty (/ base (rb/unit-factor target-unit)) target-unit))))

(define (rb/qty-str q)
  "Render a quantity as \"value unit\"."
  (string-append (as-str (qty-val q)) " " (qty-unit q)))

;; ============================================================
;; COSTS (ingredients, utilities, packaging)
;; A cost is an alist with type/name/qty_per_batch/order_qty/order_price/category
;; plus optional source/url/note/on_hand.
;; ============================================================

(define (ingredient name qbatch oqty price opts)
  "Build an ingredient cost entry."
  (rb/make-cost name qbatch oqty price "ingredient" opts))

(define (utility name qbatch oqty price opts)
  "Build a utility cost entry."
  (rb/make-cost name qbatch oqty price "utility" opts))

(define (pkg-cost name qbatch oqty price opts)
  "Build a packaging cost entry."
  (rb/make-cost name qbatch oqty price "packaging" opts))

(define (rb/make-cost name qbatch oqty price category opts)
  "Assemble a cost alist from name/quantities/price/category/opts."
  (cons (cons "type" "cost")
  (cons (cons "name" name)
  (cons (cons "qty_per_batch" qbatch)
  (cons (cons "order_qty" oqty)
  (cons (cons "order_price" price)
  (cons (cons "category" category) opts)))))))

;; opt constructors — each returns (key . value) for the opts alist
(define (source s) "Opt: source key/value." (cons "source" s))
(define (url s) "Opt: url key/value." (cons "url" s))
(define (note s) "Opt: note key/value." (cons "note" s))
(define (on-hand q) "Opt: on-hand quantity key/value." (cons "on_hand" q))

;; ============================================================
;; STEPS
;; ============================================================

(define (step name duration opts)
  "Build a process step entry."
  (cons (cons "type" "step")
  (cons (cons "name" name)
  (cons (cons "duration" duration) opts))))

(define (use name) "Opt: use-this key/value." (cons "use" name))

;; ============================================================
;; PACKAGES
;; A package is an alist with product info + packaging_costs list.
;; ============================================================

(define (package product-name units-per-batch liters-per-unit selling-price pkgs)
  "Build a package/product alist with packaging costs."
  (list
    (cons "type" "package")
    (cons "product_name" product-name)
    (cons "units_per_batch" units-per-batch)
    (cons "liters_per_unit" liters-per-unit)
    (cons "selling_price" selling-price)
    (cons "packaging_costs" pkgs)))

;; ============================================================
;; FIXED COSTS, EQUIPMENT & PLAN
;; ============================================================

(define (fixed-cost name monthly)
  "Build a fixed-cost entry."
  (list
    (cons "type" "fixed")
    (cons "name" name)
    (cons "monthly_cost" monthly)))

;; equipment: capital purchase with priority tier
(define (equipment name price priority opts)
  "Build an equipment/capital entry with priority."
  (cons (cons "type" "equipment")
  (cons (cons "name" name)
  (cons (cons "price" price)
  (cons (cons "priority" priority) opts)))))

(define (impact s) "Opt: impact key/value." (cons "impact" s))
(define (unlocks s) "Opt: unlocks key/value." (cons "unlocks" s))

(define (rb/plan batches-per-month starter)
  "Build a plan alist (batches and starter recipe)."
  ;; Renamed from unprefixed plan (i-1788685367): libs/plan defines its
  ;; own 1-param plan ("Set user plan"), and under the seed store-winner
  ;; rules that other plan would resolve at request time once the data
  ;; defines below became functions — prefix keeps rb self-contained.
  (list
    (cons "batches_per_month" batches-per-month)
    (cons "starter_recipe" starter)))

;; ============================================================
;; SHARED BLOCKS & RECIPE IMPORTS
;; ============================================================

;; A shared block is a named list of costs, reusable across recipes.
(define (shared name costs)
  "Build a reusable named block of costs."
  (list
    (cons "type" "shared")
    (cons "name" name)
    (cons "costs" costs)))

;; uses: recipe-level import of a shared block. Returns a marker that
;; rb/collect-recipe splices — the block's costs get appended to the recipe.
(define (uses block)
  "Marker importing a shared block's costs into a recipe."
  (list
    (cons "type" "uses")
    (cons "block" block)))

;; ============================================================
;; MIXED RUNS — allocate batch liters across package types
;; ============================================================

;; A mix-part is (package-name . count)
(define (mixed name parts)
  "Build a mixed-run part (package-name . count) list."
  (list
    (cons "type" "mixed")
    (cons "name" name)
    (cons "parts" parts)))

;; ============================================================
;; RECIPES — collect items into a recipe alist
;; ============================================================

(define (recipe name batch-liters items)
  "Collect items into a recipe alist with name and batch liters."
  (rb/collect-recipe
    (list (cons "name" name) (cons "batch_liters" batch-liters))
    items))

(define (rb/collect-recipe base items)
  "Partition recipe items into costs/steps/packages/mixed/uses."
  (if (null? items)
      base
    (let ((item (car items)))
      (let ((item-type (rb/get "type" item)))
        (cond
          ((string-eq item-type "cost")
           (rb/collect-recipe
             (rb/set "costs" (cons item (rb/get "costs" base)) base)
             (cdr items)))
          ((string-eq item-type "step")
           (rb/collect-recipe
             (rb/set "steps" (cons item (rb/get "steps" base)) base)
             (cdr items)))
          ((string-eq item-type "package")
           (rb/collect-recipe
             (rb/set "packages" (cons item (rb/get "packages" base)) base)
             (cdr items)))
          ((string-eq item-type "mixed")
           (rb/collect-recipe
             (rb/set "mixed" (cons item (rb/get "mixed" base)) base)
             (cdr items)))
          ((string-eq item-type "uses")
           (let ((block (rb/get "block" item)))
             (rb/collect-recipe
               (rb/set "costs"
                 (rb/append (rb/get "costs" block) (rb/get "costs" base))
                 base)
               (cdr items))))
          (else (rb/collect-recipe base (cdr items))))))))

;; ============================================================
;; CALC — business calculations (ported from calc.ml)
;; ============================================================

;; unit_cost = order_price / to_base(order_qty)
(define (rb/unit-cost c)
  "Cost per base unit of a cost entry."
  (let ((oqty-base (rb/to-base (rb/get "order_qty" c))))
    (/ (rb/get "order_price" c) oqty-base)))

;; line_cost = to_base(qty_per_batch) * unit_cost
(define (rb/line-cost c)
  "Total line cost for a cost entry's batch quantity."
  (* (rb/to-base (rb/get "qty_per_batch" c)) (rb/unit-cost c)))

;; total variable cost = sum of line costs
(define (rb/variable-cost-total costs)
  "Sum of line costs across cost entries."
  (rb/sum rb/line-cost costs))

;; fixed cost total = sum of monthly_cost
(define (rb/fixed-cost-total costs)
  "Sum of monthly fixed costs."
  (rb/sum (lambda (c) (rb/get "monthly_cost" c)) costs))

;; filter costs by category
(define (rb/costs-by-category cat costs)
  "Filter cost entries to one category."
  (rb/filter (lambda (c) (string-eq (rb/get "category" c) cat)) costs))

(define (rb/ingredient-costs costs)
  "Ingredient-cost entries only."
  (rb/costs-by-category "ingredient" costs))

(define (rb/packaging-costs costs)
  "Packaging-cost entries only."
  (rb/costs-by-category "packaging" costs))

;; ============================================================
;; BUSINESS (recipe + package option → business alist)
;; ============================================================

(define (rb/business recipe-data pkg fixed-costs)
  "Assemble a business alist from recipe, package, and fixed costs."
  (list
    (cons "product_name" (rb/get "product_name" pkg))
    (cons "units_per_batch" (rb/get "units_per_batch" pkg))
    (cons "liters_per_unit" (rb/get "liters_per_unit" pkg))
    (cons "selling_price" (rb/get "selling_price" pkg))
    (cons "batch_liters" (rb/get "batch_liters" recipe-data))
    (cons "variable_costs"
          (rb/append (rb/get "costs" recipe-data)
                     (rb/get "packaging_costs" pkg)))
    (cons "fixed_costs" fixed-costs)))

;; ============================================================
;; PER-UNIT CALCULATIONS
;; ============================================================

(define (rb/variable-cost-per-unit biz)
  "Variable cost per produced unit."
  (let ((units (rb/get "units_per_batch" biz)))
    (/ (rb/variable-cost-total (rb/get "variable_costs" biz)) units)))

(define (rb/contribution-margin-per-unit biz)
  "Selling price minus variable cost per unit."
  (- (rb/get "selling_price" biz) (rb/variable-cost-per-unit biz)))

;; ============================================================
;; MONTHLY REPORT
;; ============================================================

(define (rb/monthly-report biz pln)
  "Monthly revenue/cost/profit figures plus breakeven."
  (let ((units-per-batch (rb/get "units_per_batch" biz)))
    (let ((batches-per-month (rb/get "batches_per_month" pln)))
      (let ((units-per-month (* units-per-batch batches-per-month)))
        (let ((variable-batch (rb/variable-cost-total (rb/get "variable_costs" biz))))
          (let ((variable-unit (/ variable-batch units-per-batch)))
            (let ((fixed-month (rb/fixed-cost-total (rb/get "fixed_costs" biz))))
              (let ((revenue-month (* units-per-month (rb/get "selling_price" biz))))
                (let ((variable-month (* variable-batch batches-per-month)))
                  (let ((total-month (+ fixed-month variable-month)))
                    (let ((profit-month (- revenue-month total-month)))
                      (let ((margin-unit (- (rb/get "selling_price" biz) variable-unit)))
                        (let ((breakeven-units
                                (if (<= margin-unit 0.0) 999999.0 (/ fixed-month margin-unit))))
                          (list
                            (cons "units_per_month" units-per-month)
                            (cons "revenue_per_month" revenue-month)
                            (cons "variable_cost_per_batch" variable-batch)
                            (cons "variable_cost_per_unit" variable-unit)
                            (cons "fixed_cost_per_month" fixed-month)
                            (cons "variable_cost_per_month" variable-month)
                            (cons "total_cost_per_month" total-month)
                            (cons "profit_per_month" profit-month)
                            (cons "contribution_margin_per_unit" margin-unit)
                            (cons "breakeven_units_per_month" breakeven-units)
                            (cons "breakeven_revenue_per_month"
                                  (* breakeven-units (rb/get "selling_price" biz)))))))))))))))))

;; ============================================================
;; RECIPE SCALING (on-hand constraint)
;; ============================================================

;; scale_recipe: if any ingredient/utility has on_hand, find the limiting
;; ingredient (smallest on_hand/qty_per_batch ratio) and scale down.
(define (rb/scale-recipe recipe-data)
  "Scale a recipe down to the limiting on-hand ingredient."
  (let ((costs (rb/get "costs" recipe-data)))
    (let ((constrained (rb/filter
                          (lambda (c)
                            (and (or (string-eq (rb/get "category" c) "ingredient")
                                     (string-eq (rb/get "category" c) "utility"))
                                 (rb/has? "on_hand" c)))
                          costs)))
      (if (null? constrained)
          (list (cons "scale_factor" 1.0)
                (cons "limiting" ())
                (cons "batch_liters" (rb/get "batch_liters" recipe-data)))
        (let ((factors (rb/map
                          (lambda (c)
                            (/ (rb/to-base (rb/get "on_hand" c))
                               (rb/to-base (rb/get "qty_per_batch" c))))
                          constrained)))
          (let ((scale (rb/min-list factors)))
            (list
              (cons "scale_factor" scale)
              (cons "limiting" (rb/find-limiting costs scale))
              (cons "batch_liters"
                    (qty (* (qty-val (rb/get "batch_liters" recipe-data)) scale)
                         (qty-unit (rb/get "batch_liters" recipe-data)))))))))))

(define (rb/min-list xs)
  "Smallest element of a non-empty numeric list."
  (if (null? (cdr xs))
      (car xs)
    (let ((rest (rb/min-list (cdr xs))))
      (if (< (car xs) rest) (car xs) rest))))

(define (rb/find-limiting costs scale)
  "Name of the on-hand cost matching the given scale factor."
  (if (null? costs) ()
    (let ((c (car costs)))
      (if (rb/has? "on_hand" c)
          (let ((f (/ (rb/to-base (rb/get "on_hand" c))
                      (rb/to-base (rb/get "qty_per_batch" c)))))
            (if (= f scale)
                (rb/get "name" c)
              (rb/find-limiting (cdr costs) scale)))
        (rb/find-limiting (cdr costs) scale)))))

;; ============================================================
;; MONEY FORMATTING
;; ============================================================

(define (rb/money n)
  "Format a number as dollars-and-cents text."
  (let ((cents (round (* n 100.0))))
    (let ((whole (/ (floor cents) 100))
          (frac (mod (floor cents) 100)))
      (string-append "$"
        (string-append (as-str whole)
          (string-append "."
            (if (< frac 10)
                (string-append "0" (as-str frac))
              (as-str frac))))))))

;; ============================================================
;; OPERATING TIER — batches brewed, purchases, orders
;; (Beyond OCaml rb — closes the planned→actual loop)
;; ============================================================

;; A batch log: records an actual brew
(define (brew date recipe-name yield-liters packaged-list ferment-days carbonation rating notes)
  "Build a batch log entry for an actual brew."
  (rb/make-brew date recipe-name yield-liters packaged-list ferment-days carbonation rating notes))

(define (rb/make-brew date recipe-name yield-liters packaged-list ferment-days carbonation rating notes)
  "Assemble a batch alist from brew parameters."
  (cons (cons "type" "batch")
  (cons (cons "date" date)
  (cons (cons "recipe" recipe-name)
  (cons (cons "yield_liters" yield-liters)
  (cons (cons "packaged" packaged-list)
  (cons (cons "ferment_days" ferment-days)
  (cons (cons "carbonation" carbonation)
  (cons (cons "rating" rating)
  (cons (cons "notes" notes) (list)))))))))))

;; packaged: (cons pkg-name count) — e.g. (cons "500mL PET" 12)
(define (packaged pkg-name count) "A packaged-units entry (name . count)." (cons pkg-name count))

;; A purchase log: records an actual ingredient/equipment buy
(define (bought name qty price date opts)
  "Build a purchase log entry."
  (cons (cons "type" "purchase")
  (cons (cons "name" name)
  (cons (cons "qty" qty)
  (cons (cons "price" price)
  (cons (cons "date" date) opts))))))

;; ============================================================
;; PERSISTENCE — save recipe to the tree
;; ============================================================

(define (rb/init-storage!)
  "Ensure the recipe type exists."
  (if (null? (type-fields (quote recipe)))
      (deftype (quote recipe) (quote name) (quote batch_liters) (quote source) (quote created_day))
    ()))

(define (rb/save-recipe! recipe-data source-text)
  "Persist a recipe alist to the tree with its source text."
  (seq
    (rb/init-storage!)
    (create (quote recipe)
      (dict-set
        (dict-set
          (dict-set
            (dict-set (json-emit "{}")
              "name" (rb/get "name" recipe-data))
            "batch_liters" (as-str (rb/qty-str (rb/get "batch_liters" recipe-data))))
          "source" source-text)
        "created_day" (date-today)))))

(define (rb/recipe-rows)
  "All persisted recipe rows."
  (rows (quote recipe)))

(define (rb/fetch-recipe id)
  "Fetch a single recipe row by id."
  (get (quote recipe) id))

(define (rb/seed-all!)
  "Seed every bundled recipe, skipping ones already present."
  (seq
    (rb/init-storage!)
    (rb/seed-recipes (rb/all-recipes))))

(define (rb/seed-recipes recipes)
  "Seed a list of (name . recipe-alist) pairs that are not yet stored."
  (if (null? recipes)
      (list)
    (seq
      (let ((name (car (car recipes)))
            (r (cdr (car recipes))))
        (if (null? (rb/find-recipe-row name))
            (rb/save-recipe! r (string-append "(recipe \"" name "\" ...)"))
          ()))
      (rb/seed-recipes (cdr recipes)))))

(define (rb/find-recipe-row name)
  "Locate a stored recipe row by name; () when absent."
  (rb/find-in-rows name (rows (quote recipe))))

(define (rb/find-in-rows name rws)
  "Find a row whose name field equals name."
  (if (null? rws)
      ()
    (if (string-eq (dict-get (cdr (car rws)) "name") name)
        (car rws)
      (rb/find-in-rows name (cdr rws)))))

;; ------------------------------------------------------------
;; BREW PERSISTENCE — batches are data, not code (2026-09)
;; ------------------------------------------------------------
;; OBITUARY: the batch log used to live only as the static rb/batches
;; define (hand-transcribed from rb/batch_logs/). New brews had to be
;; lib edits + a promote. Now brews persist as data rows (type `brew`,
;; tree path data.brew.<id>), and rb/batches stays as the SEED SOURCE
;; for the historical log (rb/seed-brews!, idempotent on date+recipe).
;; v1 choice: `packaged` is stored as a display string ("12 x 500mL PET")
;; built by rb/packaged-str from the structured pairs at save time —
;; structure lives in the brew alist, flat text in the row.

(define (rb/init-brew-storage!)
  "Ensure the brew type exists."
  (if (null? (type-fields (quote brew)))
      (deftype (quote brew) (quote date) (quote recipe) (quote yield_liters) (quote packaged) (quote ferment_days) (quote carbonation) (quote rating) (quote notes) (quote created_day))
    ()))

(define (rb/packaged-str pkgs)
  "Render packaged pairs as '12 x 500mL PET' (comma-joined)."
  (if (null? pkgs)
      ""
    (let ((p (car pkgs)))
      (string-append
        (as-str (cdr p)) " x " (as-str (car p))
        (if (null? (cdr pkgs))
            ""
          (string-append ", " (rb/packaged-str (cdr pkgs))))))))

(define (rb/save-brew! b)
  "Persist a brew alist to the tree."
  (seq
    (rb/init-brew-storage!)
    (create (quote brew)
      (dict-set
        (dict-set
          (dict-set
            (dict-set
              (dict-set
                (dict-set
                  (dict-set
                    (dict-set
                      (dict-set (json-emit "{}")
                        "date" (as-str (rb/get "date" b)))
                      "recipe" (as-str (rb/get "recipe" b)))
                    "yield_liters" (as-str (rb/get "yield_liters" b)))
                  "packaged" (rb/packaged-str (rb/get "packaged" b)))
                "ferment_days" (as-str (rb/get "ferment_days" b)))
              "carbonation" (as-str (rb/get "carbonation" b)))
            "rating" (as-str (rb/get "rating" b)))
          "notes" (as-str (rb/get "notes" b)))
        "created_day" (date-today)))))

(define (rb/brew-rows)
  "All persisted brew rows."
  (rows (quote brew)))

(define (rb/find-brew-row date recipe-name)
  "Locate a stored brew row by date+recipe; () when absent."
  (rb/find-in-brews date recipe-name (rb/brew-rows)))

(define (rb/find-in-brews date recipe-name rws)
  "Find a row whose date and recipe fields both match."
  (if (null? rws)
      ()
    (if (and (string-eq (dict-get (cdr (car rws)) "date") date)
             (string-eq (dict-get (cdr (car rws)) "recipe") recipe-name))
        (car rws)
      (rb/find-in-brews date recipe-name (cdr rws)))))

(define (rb/seed-brews!)
  "Seed the historical batch log, skipping brews already stored."
  (seq
    (rb/init-brew-storage!)
    (rb/seed-brew-list (rb/batches))))

(define (rb/seed-brew-list bs)
  "Seed a list of brew alists that are not yet stored."
  (if (null? bs)
      (list)
    (seq
      (let ((b (car bs)))
        (if (null? (rb/find-brew-row (as-str (rb/get "date" b)) (as-str (rb/get "recipe" b))))
            (rb/save-brew! b)
          ()))
      (rb/seed-brew-list (cdr bs)))))

;; libs/rb/recipes.pp — all rb recipes converted to Nopales
;; Hand-converted from /home/roerick/dev/wyo.tech/rb/data/*.rb

;; ============================================================
;; SHARED BLOCKS
;; ============================================================

(define (rb/ginger-bug-base)
  (shared "ginger-bug-base"
    (list
      (ingredient "Ginger bug starter" (qty 500 "mL") (qty 1 "L") 10.00
        (list (source "Sandpoint Soda Company")
              (url "https://sandpointsodacompany.com/product/ginger-bug-soda-starter/")
              (note "Organic living soda starter, $10 (sale from $20). Make from scratch if out of stock.")))
      (utility "Filtered water" (qty 10 "L") (qty 50 "L") 4.00
        (list (note "5-gallon jugs from grocery or delivery. Must be chlorine-free. ~50c/gal.")))
      (utility "Cleaning + sanitizer" (qty 1 "batch") (qty 1 "batch") 6.00
        (list (source "Any homebrew supply")
              (url "https://www.northernbrewer.com/")
              (note "Star San or similar no-rinse sanitizer. Essential for fermentation equipment."))))))

;; ============================================================
;; RECIPES
;; ============================================================

(define (rb/simple-root-beer)
  (recipe "Simple Root Beer" (qty 6 "L")
    (list
      (ingredient "Ginger bug starter" (qty 300 "mL") (qty 1 "L") 10.00
        (list (note "~50 mL per L of batch. Active bubbly starter.")))
      (utility "Filtered water" (qty 6 "L") (qty 50 "L") 4.00
        (list (note "Chlorine-free. Must be dechlorinated for ginger bug health.")))
      (utility "Cleaning + sanitizer" (qty 1 "batch") (qty 1 "batch") 6.00
        (list (note "Star San or similar no-rinse sanitizer.")))
      (ingredient "Sassafras root bark" (qty 42 "g") (qty 450 "g") 109.02
        (list (source "Monterey Bay Herb Co")
              (note "Signature root beer flavor. Simmer with wintergreen, strain.")))
      (ingredient "Wintergreen leaf" (qty 17 "g") (qty 110 "g") 6.50
        (list (source "Penn Herb Co")
              (note "Classic root beer minty note. Simmer with sassafras.")))
      (ingredient "Vanilla extract" (qty 15 "mL") (qty 500 "mL") 12.00
        (list (note "Add at bottling to preserve aroma.")))
      (ingredient "Cane sugar" (qty 630 "g") (qty 11 "kg") 22.50
        (list (source "Any grocery")
              (note "~105 g/L - original 1876 Hires ratio (4 lb per 5 gal). Less sweet, more old-timey.")))
      (step "Steep herbs" (qty 20 "min")
        (list (use "Sassafras root bark") (use "Wintergreen leaf")
              (note "Simmer sassafras and wintergreen in ~3L water, covered, 20 min. Strain into fermenter.")))
      (step "Dissolve sugar" (qty 5 "min")
        (list (use "Cane sugar") (note "Stir sugar into hot tea until fully dissolved.")))
      (step "Cool wort" (qty 2 "hr")
        (list (note "Cool to room temp before adding starter. Speeds up with ice bath.")))
      (step "Pitch starter" (qty 10 "min")
        (list (use "Ginger bug starter") (note "Add ginger bug starter, top up to 6L with filtered water.")))
      (step "Bottle and ferment" (qty 3 "day")
        (list (note "Bottle in 12 x 500mL PET bottles. Ferment at room temp until carbonated (2-4 days). Squeeze-test for pressure.")))
      (step "Add vanilla" (qty 5 "min")
        (list (use "Vanilla extract") (note "Add vanilla extract at bottling to preserve aroma.")))
      (package "500mL PET" 12 (qty 0.5 "L") 7.00
        (list
          (pkg-cost "PET bottles with caps" (qty 12 "each") (qty 96 "each") 112.32
            (list (source "KegLand") (note "Reusable PET beer bottles, 500mL. $1.17/bottle after 10% volume discount.")))
          (pkg-cost "Premium labels" (qty 12 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule") (note "Waterproof BOPP film roll labels.")))
          (pkg-cost "Breakage reserve" (qty 12 "each") (qty 96 "each") 5.00
            (list (note "~5% replacement rate for caps/gaskets."))))))))

(define (rb/root-beer)
  (recipe "Ginger Bug Root Beer" (qty 10 "L")
    (list
      (uses (rb/ginger-bug-base))
      (ingredient "Sarsaparilla root" (qty 0.42 "lb") (qty 1 "lb") 22.00
        (list (source "Starwest Botanicals")
              (url "https://www.starwest-botanicals.com/sarsaparilla-root-whole-csw.html")
              (note "Sarsaparilla Root Whole Cut & Sifted. ~$7.33/4oz, ~$22/lb. Jamaican sarsaparilla also works.")))
      (ingredient "Sassafras root bark" (qty 0.16 "lb") (qty 1 "lb") 109.02
        (list (source "Monterey Bay Herb Co")
              (url "https://www.montereybayherb.com/product/sassafras-root-bark-cut-and-sifted/")
              (note "Real sassafras - legal under Wyoming Food Freedom Act (Wyo. Stat. 35-10-301). The signature root beer flavor.")))
      (ingredient "Wintergreen leaf" (qty 0.06 "lb") (qty 4 "oz") 6.50
        (list (source "Penn Herb Co")
              (url "https://www.pennherb.com/wintergreen-leaves-cut-4-oz-456C4")
              (note "Wintergreen Leaves, Cut (Gaultheria procumbens). Wild harvested. Provides the classic root beer flavor note.")))
      (ingredient "Vanilla beans" (qty 0.05 "lb") (qty 4 "oz") 30.00
        (list (source "Natural Vanilla USA")
              (url "https://naturalvanilla.us/vanilla-beans/")
              (note "Grade B (extract grade) Madagascar Bourbon. Best value for infusion/brewing.")))
      (ingredient "Molasses" (qty 0.31 "gal") (qty 1 "gal") 28.00
        (list (source "WebstaurantStore")
              (url "https://www.webstaurantstore.com/grandmas-unsulfured-molasses-1-gallon/9997971942.html")
              (note "Grandma's Unsulfured Original Molasses, 1 gallon.")))
      (ingredient "Cane sugar" (qty 5.0 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (url "https://www.foodservicedirect.com/esome-organic-cane-sugar-25-pound-1-per-case-172716.html")
              (note "Wholesome Organic Cane Sugar 25 lb bag. Conventional ~$0.50/lb. Organic ~$2/lb.")))
      (package "1L Flip-Top" 10 (qty 1.0 "L") 18.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 10 "each") (qty 12 "each") 89.99
            (list (source "Northern Brewer")
                  (url "https://www.northernbrewer.com/products/ez-cap-bottles-brown-1-liter-with-flip-tops")
                  (note "Brown EZ Cap 1L swing top bottles, 12/case. Reusable with replacement gaskets.")))
          (pkg-cost "Premium labels" (qty 10 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")
                  (url "https://www.stickermule.com/products/die-cut-roll-labels")
                  (note "Waterproof BOPP film roll labels. 100 for $66. Dishwasher safe.")))
          (pkg-cost "Breakage reserve" (qty 10 "each") (qty 12 "each") 5.00
            (list (note "Budget ~2% of bottle cost per batch for breakage/replacement gaskets.")))))
      (package "500mL Flip-Top" 20 (qty 0.5 "L") 12.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 20 "each") (qty 12 "each") 69.99
            (list (source "Northern Brewer")
                  (url "https://www.northernbrewer.com/products/ez-cap-bottles-brown-1-liter-with-flip-tops")
                  (note "Brown EZ Cap 16oz (~500mL) swing top bottles, 12/case.")))
          (pkg-cost "Premium labels" (qty 20 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")
                  (url "https://www.stickermule.com/products/die-cut-roll-labels")))
          (pkg-cost "Breakage reserve" (qty 20 "each") (qty 12 "each") 5.00
            (list (note "Breakage budget.")))))
      (mixed "Half 1L + Half 500mL"
        (list (cons "1L Flip-Top" 5) (cons "500mL Flip-Top" 10))))))

(define (rb/ginger-ale)
  (recipe "Ginger Ale" (qty 10 "L")
    (list
      (uses (rb/ginger-bug-base))
      (ingredient "Fresh ginger" (qty 1.5 "lb") (qty 1 "lb") 5.50
        (list (source "Any grocery / Asian market")
              (note "For syrup method: grate or slice thin, simmer with sugar and water, strain before mixing. Start with 1.5 lb for 10L, adjust to taste.")))
      (ingredient "Cane sugar" (qty 2.2 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (note "Dissolve in ginger syrup or add directly. ~100g/L is the standard ginger ale ratio. Adjust based on taste tests.")))
      (ingredient "Lemon juice" (qty 0.25 "cup") (qty 1 "gal") 8.00
        (list (source "Any grocery")
              (note "Bottled or fresh. For acidity balance. Start conservative, add more if too sweet.")))
      (ingredient "Black tea" (qty 2 "each") (qty 20 "each") 5.00
        (list (source "Any grocery")
              (note "2 tea bags for tannin/body. Optional - omit for lighter style.")))
      (package "1L Flip-Top" 10 (qty 1.0 "L") 16.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 10 "each") (qty 12 "each") 89.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 10 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 10 "each") (qty 12 "each") 5.00
            (list))))
      (package "500mL Flip-Top" 20 (qty 0.5 "L") 10.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 20 "each") (qty 12 "each") 69.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 20 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 20 "each") (qty 12 "each") 5.00
            (list))))
      (mixed "Market mix"
        (list (cons "1L Flip-Top" 4) (cons "500mL Flip-Top" 12))))))

(define (rb/lemonade)
  (recipe "Ginger Bug Lemonade" (qty 10 "L")
    (list
      (uses (rb/ginger-bug-base))
      (ingredient "Fresh lemons" (qty 20 "each") (qty 20 "each") 18.00
        (list (source "Any grocery / Costco")
              (note "Bag of 20 lemons ~$18. Organic costs more. Check farmers markets for better pricing.")))
      (ingredient "Fresh ginger" (qty 1.2 "lb") (qty 1 "lb") 5.50
        (list (source "Any grocery / Asian market")
              (note "Fresh ginger root. Asian markets often have best bulk pricing (~$3-5/lb). Also feeds the ginger bug.")))
      (ingredient "Cane sugar" (qty 4.5 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (url "https://www.foodservicedirect.com/esome-organic-cane-sugar-25-pound-1-per-case-172716.html")
              (note "Same source as root beer recipe.")))
      (package "1L Flip-Top" 10 (qty 1.0 "L") 16.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 10 "each") (qty 12 "each") 89.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 10 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 10 "each") (qty 12 "each") 5.00
            (list))))
      (package "500mL Flip-Top" 20 (qty 0.5 "L") 10.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 20 "each") (qty 12 "each") 69.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 20 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 20 "each") (qty 12 "each") 5.00
            (list))))
      (mixed "Market mix"
        (list (cons "1L Flip-Top" 4) (cons "500mL Flip-Top" 12))))))

(define (rb/cola)
  (recipe "Ginger Bug Cola" (qty 10 "L")
    (list
      (uses (rb/ginger-bug-base))
      (ingredient "Kola nut" (qty 0.35 "lb") (qty 1 "lb") 24.00
        (list (source "American Bulk Herbs / KolaNutWholesale")
              (url "https://www.kolanutwholesale.com/")
              (note "Cola nitida (white or red variety). Small quantities $20-40/lb. Also at Starwest Botanicals and some African grocery stores.")))
      (ingredient "Cinnamon" (qty 0.08 "lb") (qty 1 "lb") 14.00
        (list (source "Any grocery / Monterey Bay Herb Co")
              (url "https://www.herbco.com/")
              (note "Ceylon cinnamon sticks or cut. Cassia is cheaper (~$6/lb) but Ceylon is better for cola.")))
      (ingredient "Orange peel" (qty 0.15 "lb") (qty 1 "lb") 11.00
        (list (source "Monterey Bay Herb Co")
              (url "https://www.herbco.com/")
              (note "Dried orange peel cut & sifted. Also at Mountain Rose Herbs, Starwest Botanicals.")))
      (ingredient "Lemon peel" (qty 0.10 "lb") (qty 1 "lb") 10.00
        (list (source "Monterey Bay Herb Co")
              (url "https://www.herbco.com/")
              (note "Dried lemon peel cut & sifted. Also at Mountain Rose Herbs, Starwest Botanicals.")))
      (ingredient "Vanilla beans" (qty 0.04 "lb") (qty 4 "oz") 30.00
        (list (source "Natural Vanilla USA")
              (url "https://naturalvanilla.us/vanilla-beans/")
              (note "Grade B Madagascar Bourbon. Best value for infusion.")))
      (ingredient "Cane sugar" (qty 5.5 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (note "Same source as other recipes.")))
      (ingredient "Molasses" (qty 0.20 "gal") (qty 1 "gal") 28.00
        (list (source "WebstaurantStore")
              (url "https://www.webstaurantstore.com/grandmas-unsulfured-molasses-1-gallon/9997971942.html")
              (note "Grandma's Unsulfured Original, 1 gallon.")))
      (package "1L Flip-Top" 10 (qty 1.0 "L") 17.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 10 "each") (qty 12 "each") 89.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 10 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 10 "each") (qty 12 "each") 5.00
            (list))))
      (package "500mL Flip-Top" 20 (qty 0.5 "L") 11.00
        (list
          (pkg-cost "Flip-top glass bottles" (qty 20 "each") (qty 12 "each") 69.99
            (list (source "Northern Brewer")))
          (pkg-cost "Premium labels" (qty 20 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule")))
          (pkg-cost "Breakage reserve" (qty 20 "each") (qty 12 "each") 5.00
            (list))))
      (mixed "Balanced split"
        (list (cons "1L Flip-Top" 5) (cons "500mL Flip-Top" 10))))))

(define (rb/happy-ginger)
  (recipe "Happy Ginger Ginger Beer" (qty 9.6 "L")
    (list
      (ingredient "Ginger bug starter" (qty 480 "mL") (qty 1 "L") 0.00
        (list (note "Home-made ginger bug. 1 L on hand shared with Traditional Root Beer tonight. Save ~40 mL as next starter first, then split the remaining 960 mL 50/50 -> 480 mL each. ~50 mL/L = normal pitch rate. Sunk cost - ginger/sugar that fed it tracked below (spent + fresh ginger, cane sugar), not here.")))
      (utility "Filtered water" (qty 9.6 "L") (qty 50 "L") 4.00
        (list (note "Chlorine-free, dechlorinated for bug health. 5-gallon jugs from grocery/delivery. ~50c/gal. Top up to 9.6 L batch volume.")))
      (utility "Cleaning + sanitizer" (qty 1 "batch") (qty 1 "batch") 6.00
        (list (source "Any homebrew supply")
              (url "https://www.northernbrewer.com/")
              (note "Star San or similar no-rinse sanitizer. Essential for fermentation equipment. One sanitizer cycle can cover both carboys tonight.")))
      (ingredient "Spent ginger (from bug)" (qty 300 "g") (qty 1 "lb") 0.00
        (list (note "ALL 300 g spent ginger on hand goes here. Strained out of the ginger bug before pitching. Already extracted by the bug - body/residual ginger flavor, not bright heat. Free ingredient (sunk into the bug).")))
      (ingredient "Fresh ginger" (qty 200 "g") (qty 1 "lb") 5.50
        (list (source "Any grocery / Asian market")
              (note "ALL 200 g fresh ginger on hand goes here. Bright heat. Total ginger = 300 g spent + 200 g fresh = 500 g ~ 52 g/L - right on brew 3's 54 g/L. Grate or slice thin, simmer with spent ginger, strain before mixing.")))
      (ingredient "Cane sugar" (qty 2.22 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (note "1008 g total = 105 g/L target (brew 3 ratio, rated 9/10). Residual after ferment will be lower - BACKSWEETEN at bottling, do not skip.")))
      (ingredient "Lime juice" (qty 0.64 "cup") (qty 1 "gal") 8.00
        (list (source "Any grocery")
              (note "~150 mL = juice of ~5 limes for 9.6 L. Add at bottling to preserve freshness. Taste and adjust.")))
      (ingredient "Pineapple juice" (qty 0.64 "cup") (qty 1 "gal") 8.00
        (list (source "Any grocery")
              (note "~150 mL. Pairs with lime. Add at bottling. Taste and adjust.")))
      (ingredient "Black tea" (qty 2 "each") (qty 20 "each") 5.00
        (list (source "Any grocery")
              (note "2 tea bags for tannin/body. Optional - omit for lighter style.")))
      (step "Steep ginger" (qty 20 "min")
        (list (use "Fresh ginger") (use "Spent ginger (from bug)")
              (note "Simmer grated/sliced fresh ginger WITH spent bug ginger in ~3 L water, covered, 20 min. Strain into fermenter. The fresh ginger is where the bright heat comes from - don't skip the simmer.")))
      (step "Steep tea" (qty 5 "min")
        (list (use "Black tea") (note "Steep tea bags in the hot ginger tea off the heat, 5 min, then remove bags.")))
      (step "Dissolve sugar" (qty 5 "min")
        (list (use "Cane sugar") (note "Stir 1008 g sugar into hot tea until fully dissolved.")))
      (step "Cool wort" (qty 2 "hr")
        (list (note "Cool to room temp before adding bug. Ice bath speeds this up. Same window as the root beer cool - stagger or ice-bath both.")))
      (step "Pitch bug" (qty 10 "min")
        (list (use "Ginger bug starter") (note "Pitch 480 mL ginger bug. Top up to 9.6 L with filtered water. Normal 50 mL/L pitch rate.")))
      (step "Primary ferment" (qty 2 "day")
        (list (note "Ferment at room temp 2-3 days until actively bubbly and sweetness has dropped. Taste - should read dry/tart, not soda-sweet. That dryness is expected.")))
      (step "Backsweeten, citrus, bottle" (qty 20 "min")
        (list (use "Lime juice") (use "Pineapple juice")
              (note "CRITICAL - load-bearing for the 9/10 result. In the fermenter: dissolve extra cane sugar to taste (start ~3/4 cup / ~150 g for 9.6 L, taste, add more). Add lime + pineapple (~150 mL each), stir evenly. Bottle into 19 x 500 mL PET. Fridge when firm, or fridge immediately for lighter carb. Drink within 1-2 weeks. Lime/pineapple also add ~12 g and ~20 g fermentable sugar on top of the cane.")))
      (step "Carbonation watch" (qty 2 "day")
        (list (note "If carbing at room temp after backsweetening: squeeze-test PET daily. When firm, REFRIGERATE IMMEDIATELY. Ginger grenade risk is real.")))
      (package "500mL PET" 19 (qty 0.5 "L") 5.00
        (list
          (pkg-cost "PET bottles with caps" (qty 19 "each") (qty 96 "each") 112.32
            (list (source "KegLand") (note "Reusable PET beer bottles, 500mL. $1.17/bottle after 10% volume discount.")))
          (pkg-cost "Premium labels" (qty 19 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule") (note "Waterproof BOPP film roll labels.")))
          (pkg-cost "Breakage reserve" (qty 19 "each") (qty 96 "each") 5.00
            (list (note "~5% replacement rate for caps/gaskets."))))))))

(define (rb/traditional-root-beer)
  (recipe "Traditional Root Beer" (qty 9.6 "L")
    (list
      (ingredient "Ginger bug starter" (qty 480 "mL") (qty 1 "L") 0.00
        (list (note "Home-made ginger bug. 1 L on hand shared with Happy Ginger tonight. Save ~40 mL as next starter first, then split the remaining 960 mL 50/50 -> 480 mL each. ~50 mL/L. Sunk cost - not tracked here.")))
      (utility "Filtered water" (qty 9.6 "L") (qty 50 "L") 4.00
        (list (note "Chlorine-free, dechlorinated for bug health. Steep was 4 L; top up to 9.6 L batch volume after pitching.")))
      (utility "Cleaning + sanitizer" (qty 1 "batch") (qty 1 "batch") 6.00
        (list (source "Any homebrew supply")
              (url "https://www.northernbrewer.com/")
              (note "Star San or similar no-rinse sanitizer. One sanitizer cycle can cover both carboys tonight.")))
      (ingredient "Sassafras root bark" (qty 77 "g") (qty 454 "g") 128.99
        (list (source "Mountain Rose Herbs")
              (note "As brewed 2026-07-21. Signature root beer flavor. Simmer with hard roots. Legal under Wyoming Food Freedom Act (Wyo. Stat. 35-10-301).")))
      (ingredient "Sarsaparilla root" (qty 77 "g") (qty 454 "g") 31.98
        (list (note "As brewed 2026-07-21. Equal bill with sassafras. Earthy sweetness + foamy head (saponins).")))
      (ingredient "Wintergreen leaf" (qty 44 "g") (qty 454 "g") 39.78
        (list (source "Mountain Rose Herbs")
              (note "As brewed 2026-07-21. Less than sassafras by weight (44 g vs 77 g). Modern root beer mint note - commercial root beer leans wintergreen after the sassafras ban. Simmer with hard roots.")))
      (ingredient "Licorice root" (qty 31 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op")
              (note "As brewed 2026-07-21. Whole licorice root. FORWARD flavor - glycyrrhizin = sweet + mouth-coating. Simmer full steep; whole root needs the extraction time.")))
      (ingredient "Star anise" (qty 4 "each") (qty 1 "each") 8.00
        (list (note "As brewed 2026-07-21. 4 pods. Supports licorice with anise aroma. Within published range (Serious Eats uses 3 pods/gal).")))
      (ingredient "Ashwagandha root" (qty 44 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op")
              (note "As brewed 2026-07-21. Back in from v1. Earthy baseline; secondary to sassafras/sarsaparilla/wintergreen/licorice.")))
      (ingredient "Echinacea root" (qty 13 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op")
              (note "As brewed 2026-07-21. Light supporting root. Simmer with hard roots.")))
      (ingredient "Cane sugar" (qty 2.33 "lb") (qty 25 "lb") 22.50
        (list (source "Any grocery / FoodServiceDirect")
              (note "1056 g = 110 g/L pitched. No molasses - cane only. Bug eats most in 2-4 days; residual goes dry/tart. MUST backsweeten at bottling or you get the sour note from v1.")))
      (ingredient "Vanilla extract" (qty 10 "mL") (qty 500 "mL") 12.00
        (list (note "Add at bottling to preserve aroma. ~2 tsp for 9.6 L. Rounds the roots. Third leg of modern root beer (wintergreen + licorice + vanilla).")))
      (step "Steep hard roots" (qty 20 "min")
        (list (use "Sassafras root bark") (use "Sarsaparilla root") (use "Wintergreen leaf")
              (use "Licorice root") (use "Star anise") (use "Ashwagandha root") (use "Echinacea root")
              (note "As brewed 2026-07-21: all listed herbs simmered in 4 L water, covered. Hard roots + wintergreen + whole licorice + star anise + ashwagandha + echinacea together. Strain through cheesecloth into fermenter, pressing to extract. Wort dark amber.")))
      (step "Dissolve sugar" (qty 5 "min")
        (list (use "Cane sugar")
              (note "Stir 1056 g cane sugar into the hot herbal tea until fully dissolved. No molasses - do not add any. Color comes from the roots.")))
      (step "Cool wort" (qty 2 "hr")
        (list (note "Cool to room temp before adding bug. Ice bath speeds this to ~30-60 min. Pitching above ~100F / 38C kills the bug. Same window as ginger beer cool - stagger or ice-bath both.")))
      (step "Pitch bug" (qty 10 "min")
        (list (use "Ginger bug starter")
              (note "Pitch 480 mL ginger bug. Top up to 9.6 L with filtered water. ~50 mL/L. Bug must be actively bubbling.")))
      (step "Primary ferment" (qty 2 "day")
        (list (note "Ferment at room temp 2-3 days until actively bubbly and sweetness has dropped. Taste - should read dry/tart, not soda-sweet. That dryness is expected; backsweetening fixes it. Do not go bone-dry for a week or the sour note takes over.")))
      (step "Backsweeten, vanilla, bottle" (qty 20 "min")
        (list (use "Vanilla extract")
              (note "CRITICAL - load-bearing for not-sour root beer (same lesson as Happy Ginger brew 3, 9/10). In the fermenter: dissolve extra cane sugar to taste (start ~3/4-1 cup / ~150-200 g for 9.6 L, stir, taste, add more). Add 10 mL vanilla, stirred evenly. Bottle into 19 x 500 mL PET. Fridge when carb is where you want it. Once backsweetened, bug restarts in bottle. Drink within 1-2 weeks, or pasteurize (165F / 74C x 10 min) after 1-2 days carb to lock it.")))
      (step "Carbonation watch" (qty 2 "day")
        (list (note "If carbing at room temp after backsweetening: squeeze-test PET daily. When firm, REFRIGERATE IMMEDIATELY. Ginger grenade risk is real with backsweetened bug soda.")))
      (package "500mL PET" 19 (qty 0.5 "L") 7.00
        (list
          (pkg-cost "PET bottles with caps" (qty 19 "each") (qty 96 "each") 112.32
            (list (source "KegLand") (note "Reusable PET beer bottles, 500mL. $1.17/bottle after 10% volume discount.")))
          (pkg-cost "Premium labels" (qty 19 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule") (note "Waterproof BOPP film roll labels.")))
          (pkg-cost "Breakage reserve" (qty 19 "each") (qty 96 "each") 5.00
            (list (note "~5% replacement rate for caps/gaskets."))))))))

(define (rb/experimental-004)
  (recipe "Experimental Root Beer 004: 1896 edition" (qty 6 "L")
    (list
      (ingredient "Ginger bug starter" (qty 300 "mL") (qty 1 "L") 0.00
        (list (note "Home-made ginger bug. ~50 mL/L pitch.")))
      (utility "Filtered water" (qty 6 "L") (qty 50 "L") 4.00
        (list (note "Chlorine-free, dechlorinated for bug health.")))
      (utility "Cleaning + sanitizer" (qty 1 "batch") (qty 1 "batch") 6.00
        (list (note "Star San or similar no-rinse sanitizer.")))
      (ingredient "Sassafras root bark" (qty 42 "g") (qty 454 "g") 128.99
        (list (source "Mountain Rose Herbs") (note "Signature root beer flavor. Legal under Wyoming Food Freedom Act.")))
      (ingredient "Sarsaparilla root" (qty 40 "g") (qty 454 "g") 31.98
        (list (note "Earthy sweetness + foamy head.")))
      (ingredient "Wintergreen leaf" (qty 17 "g") (qty 454 "g") 39.78
        (list (source "Mountain Rose Herbs") (note "Classic root beer mint note.")))
      (ingredient "Licorice root" (qty 25 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05 with the kitchen-sink haul. Whole root.")))
      (ingredient "Ashwagandha root" (qty 30 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05. Earthy baseline.")))
      (ingredient "Echinacea root" (qty 20 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05. Supporting root.")))
      (ingredient "Spearmint leaf" (qty 15 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05. Extra mint on top of wintergreen.")))
      (ingredient "Chamomile flower" (qty 10 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05. Soft floral roundness.")))
      (ingredient "Hyssop herb" (qty 10 "g") (qty 300 "g") 5.00
        (list (source "Local food co-op") (note "Bought 2026-07-05. Bitter/herbal edge.")))
      (ingredient "Molasses" (qty 0.15 "gal") (qty 1 "gal") 28.00
        (list (note "THE off-flavor culprit in post-mortem. Present in this batch; dropped entirely in Traditional Root Beer v2.")))
      (ingredient "Cane sugar" (qty 630 "g") (qty 11 "kg") 22.50
        (list (source "Any grocery") (note "~105 g/L base fermentables, plus molasses.")))
      (ingredient "Vanilla extract" (qty 15 "mL") (qty 500 "mL") 12.00
        (list (note "Add at bottling to preserve aroma.")))
      (step "Steep kitchen sink" (qty 25 "min")
        (list (use "Sassafras root bark") (use "Sarsaparilla root") (use "Wintergreen leaf")
              (use "Licorice root") (use "Ashwagandha root") (use "Echinacea root")
              (use "Spearmint leaf") (use "Chamomile flower") (use "Hyssop herb")
              (note "Everything in the pot. Simmer covered ~20-25 min in ~3 L water. Strain, press.")))
      (step "Dissolve sugars" (qty 5 "min")
        (list (use "Cane sugar") (use "Molasses")
              (note "Stir cane sugar + molasses into hot tea until dissolved.")))
      (step "Cool wort" (qty 2 "hr")
        (list (note "Cool to room temp before pitching. Ice bath helps.")))
      (step "Pitch bug" (qty 10 "min")
        (list (use "Ginger bug starter") (note "Pitch ~300 mL bug, top up to 6 L.")))
      (step "Primary ferment" (qty 3 "day")
        (list (note "Room temp until actively bubbly and sweetness drops.")))
      (step "Vanilla and bottle" (qty 15 "min")
        (list (use "Vanilla extract") (note "Vanilla at bottling. 12 x 500 mL PET.")))
      (package "500mL PET" 12 (qty 0.5 "L") 7.00
        (list
          (pkg-cost "PET bottles with caps" (qty 12 "each") (qty 96 "each") 112.32
            (list (source "KegLand") (note "Reusable PET beer bottles, 500mL.")))
          (pkg-cost "Premium labels" (qty 12 "each") (qty 100 "each") 20.00
            (list (source "Sticker Mule") (note "Waterproof BOPP film roll labels.")))
          (pkg-cost "Breakage reserve" (qty 12 "each") (qty 96 "each") 5.00
            (list (note "~5% replacement rate for caps."))))))))

;; ============================================================
;; BUSINESS CONFIG (from business.rb)
;; ============================================================

(define (rb/fixed-costs)
  (list
    (fixed-cost "Booth rent" 60.00)))

(define (rb/equipment-list)
  (list
    (equipment "2-gal glass jar with airlock" 23.58 1
      (list (impact "Ferment a 10L batch in one vessel")
            (unlocks "First batch")
            (source "Walmart")
            (url "https://www.walmart.com/ip/2-Gallon-Primary-Fermenter-with-Lid-and-Airlock/654980559")))
    (equipment "Nylon strainer bags (2-pack)" 13.98 1
      (list (impact "Steep herbs without mess, easy cleanup")
            (unlocks "Clean root beer production")
            (source "Northern Brewer")
            (url "https://www.northernbrewer.com/products/nylon-hop-bag-8-x-9-1-4")))
    (equipment "Auto-siphon + tubing" 19.99 1
      (list (impact "Sanitary transfer, no contamination risk. Saves 1 spoiled batch/month.")
            (unlocks "Reliable fermentation")
            (source "Northern Brewer")
            (url "https://www.northernbrewer.com/products/auto-siphon-3-flags")))
    (equipment "Hydrometer" 8.79 1
      (list (impact "Know when fermentation is done, consistent carbonation")
            (unlocks "Consistent product")
            (source "MoreBeer")
            (url "https://morebeer.com/products/triple-scale-hydrometer-specific-gravity-potential-alcohol-brix")))
    (equipment "Folding digital thermometer" 12.49 1
      (list (impact "Brew temp control for ginger bug health")
            (unlocks "Consistent fermentation")
            (source "MoreBeer")
            (url "https://morebeer.com/products/folding-instant-read-digital-thermometer")))))

(define (rb/plan-config)
  (rb/plan 4 "Ginger Bug Root Beer"))

;; ============================================================
;; OPERATING LOG (from batch_logs/ and orders/)
;; ============================================================

(define (rb/batches)
  (list
    (brew "2026-06-05" "Ginger Ale" 3.16 (list (packaged "500mL PET" 8)) 3 "light" 6
      "Ginger bug was weak, carbonation suffered. Too sweet (145 g/L). Lemon+vanilla combo was iffy. Some said 'tastes like Chinese food but in the best way.' Next time: reduce ginger to 180g, cut sugar, swap lemon for lime or pineapple.")
    (brew "2026-06-12" "Simple Root Beer" 6.0 (list (packaged "500mL" 12)) 0 () 0
      "First batch. Sassafras + wintergreen + vanilla extract. No molasses, no sarsaparilla. Old-school root beer profile.")
    (brew "2026-07-11" "Happy Ginger Ginger Beer" 12.0 (list (packaged "500mL PET" 24)) 3 () 9
      "Fresh+spent ginger hypothesis confirmed - clean ginger heat, no 'Chinese food' note from brew 1. Lime and pineapple are subtle. Sugar at 105 g/L landed well. Yield 12L vs 9.44L planned (27% over). BACKSWEETENED at bottling - load-bearing for 9/10 result.")
    (brew "2026-07-12" "Experimental Root Beer 004: 1896 edition" 6.0 (list (packaged "500mL PET" 12)) 7 () 7
      "Brewed 2026-07-05 (kitchen-sink herb haul); bottled ~2026-07-12. Molasses later diagnosed as 100% the off-flavor (dropped in Traditional Root Beer v2). Selling as Experimental 004 / 1896 edition.")))

(define (rb/purchases)
  (list
    (bought "Sassafras root bark" (qty 454 "g") 128.99 "2026-06-12"
      (list (source "Mountain Rose Herbs") (note "SKU 11-00466-17. $112.50 + $16.49 shipping. Cut & sifted sassafras bark.")))
    (bought "Wintergreen leaf" (qty 454 "g") 39.78 "2026-06-14"
      (list (source "Mountain Rose Herbs") (note "2 x 8 oz bags. Product ID 576812558. $19.89 each, free shipping.")))
    (bought "500mL PET bottles" (qty 96 "each") 112.32 "2026-06-14"
      (list (source "KegLand") (note "4 cases of 24. $119.96 - 10% discount = $108.00 + $4.32 tax. Free shipping.")))
    (bought "Echinacea root" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))
    (bought "Ashwagandha root" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))
    (bought "Spearmint leaf" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))
    (bought "Chamomile flower" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))
    (bought "Licorice root" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))
    (bought "Hyssop herb" (qty 300 "g") 5.00 "2026-07-05"
      (list (source "Local food co-op bulk bins") (note "Co-op bulk-bin pricing estimate ($1.67/100g).")))))

;; ============================================================
;; ALL RECIPES registry
;; ============================================================

(define (rb/all-recipes)
  (list
    (cons "Simple Root Beer" (rb/simple-root-beer))
    (cons "Ginger Bug Root Beer" (rb/root-beer))
    (cons "Ginger Ale" (rb/ginger-ale))
    (cons "Ginger Bug Lemonade" (rb/lemonade))
    (cons "Ginger Bug Cola" (rb/cola))
    (cons "Happy Ginger Ginger Beer" (rb/happy-ginger))
    (cons "Traditional Root Beer" (rb/traditional-root-beer))
    (cons "Experimental Root Beer 004: 1896 edition" (rb/experimental-004))))

;; lookup recipe by name
(define (rb/find-recipe name)
  "Look up a recipe alist by name."
  (rb/get name (rb/all-recipes)))

;; calc one recipe with first package option + fixed costs
;; returns (name . report) or (name . error-string)
(define (rb/calc-one name)
  "One recipe's monthly report (or an error string)."
  (let ((r (rb/find-recipe name)))
    (if (null? r)
        (cons name "not found")
      (let ((pkgs (rb/get "packages" r)))
        (if (null? pkgs)
            (cons name "no packages")
          (let ((biz (rb/business r (car pkgs) (rb/fixed-costs))))
            (cons name (rb/monthly-report biz (rb/plan-config)))))))))

;; calc all recipes -> list of (name . report)
(define (rb/calc-all)
  "Monthly report for every recipe."
  (rb/map (lambda (pair) (rb/calc-one (car pair))) (rb/all-recipes)))
;; libs/rb/ui.pp — rb route handlers (htmx, server-rendered)
;;
;; OBITUARY (2026-09): the handlers used to open with (load-library "rb").
;; That predates lazy env (per-identity sessions bind package defs from the
;; branch chain on attach, bodies force on first touch), and it was actively
;; harmful on garden branches: load-library re-evals the DEPLOYED release
;; file onto the CURRENT branch, so a personal-branch pin of a changed
;; handler got clobbered back to the release body on the handler's own
;; next page load (observed live 2026-09-07: /batches reverted mid-render).
;; notes/todo never load-library in handlers — house style is no
;; load-library in route handlers. Removed from all nine.
;; Depends on: libs/rb/lib.pp, libs/rb/recipes.pp, html package

;; ============================================================
;; HTML HELPERS
;; ============================================================

(define (rb/h1 s) "Render an h1." (html/h1 s))
(define (rb/h2 s) "Render an h2." (html/h2 s))
(define (rb/h3 s) "Render an h3." (html/h3 s))
(define (rb/p s) "Render a paragraph." (html/tag "p" "" s))
(define (rb/td s) "Render a table cell." (html/tag "td" "" s))
(define (rb/th s) "Render a table header cell." (html/tag "th" "" s))

(define (rb/tr cells) "Render a table row from a list of cells." (html/tag "tr" "" (html/join cells)))
(define (rb/table rows) "Render a styled table." (html/tag "table" (html/attr "class" "rb-table") (html/join rows)))

(define (rb/li s) "Render a list item." (html/tag "li" "" s))
(define (rb/ul items) "Render a list from items." (html/tag "ul" "" (html/join items)))

(define (rb/card title body)
  "Render a titled card around a body."
  (html/div (html/attr "class" "card")
    (html/join (list (html/h3 title) body))))

(define (rb/link href text)
  "Render an anchor with href."
  (html/tag "a" (html/attr "href" href) text))

(define (rb/money-cell n)
  "Render a money value as a table cell."
  (rb/td (rb/money n)))

;; ============================================================
;; RECIPE LIST PAGE — /recipes
;; ============================================================

(define (rb/page-recipes)
  "Recipes index page route handler."
  (seq
    (html/page
      (list
        (rb/h1 "Recipes")
        (rb/table
          (rb/map rb/recipe-row (rb/all-recipes)))))))

(define (rb/recipe-row pair)
  "HTML table row for one recipe with key figures."
  (let ((name (car pair))
        (r (cdr pair)))
    (let ((report (cdr (rb/calc-one name))))
      (rb/tr (list
        (rb/td (rb/link (string-append "/recipe?name=" (html-escape name)) name))
        (rb/td (rb/qty-str (rb/get "batch_liters" r)))
        (rb/td (as-str (rb/count (rb/get "costs" r))))
        (rb/money-cell (rb/get "variable_cost_per_unit" report))
        (rb/money-cell (rb/get "contribution_margin_per_unit" report))
        (rb/money-cell (rb/get "profit_per_month" report)))))))

;; ============================================================
;; RECIPE DETAIL PAGE — /recipe?name=...
;; ============================================================

(define (rb/page-recipe-detail)
  "Recipe detail page route handler."
  (seq
    (let ((name (request-param "name")))
      (if (null? name)
          (html/page (list (rb/h1 "Recipe not found")))
        (let ((r (rb/find-recipe (as-str name))))
          (if (null? r)
              (html/page (list (rb/h1 "Recipe not found")))
            (html/page
              (list
                (rb/h1 (rb/get "name" r))
                (rb/recipe-detail-body r)))))))))

(define (rb/recipe-detail-body r)
  "Body of the recipe detail page (ingredients/steps/packages/report)."
  (html/join
    (list
      (rb/h2 "Ingredients")
      (rb/table (rb/ingredient-rows r))
      (rb/h2 "Steps")
      (rb/step-list r)
      (rb/h2 "Packages")
      (rb/table (rb/package-rows r))
      (rb/h2 "Monthly Report")
      (rb/report-table r))))

(define (rb/ingredient-rows r)
  "Header plus one row per ingredient cost."
  (let ((costs (rb/get "costs" r)))
    (cons
      (rb/tr (list (rb/th "Name") (rb/th "Per batch") (rb/th "Order") (rb/th "Price") (rb/th "Line cost") (rb/th "Category")))
      (rb/map rb/ingredient-row costs))))

(define (rb/ingredient-row c)
  "HTML row for a single cost entry."
  (rb/tr (list
    (rb/td (rb/get "name" c))
    (rb/td (rb/qty-str (rb/get "qty_per_batch" c)))
    (rb/td (rb/qty-str (rb/get "order_qty" c)))
    (rb/money-cell (rb/get "order_price" c))
    (rb/money-cell (rb/line-cost c))
    (rb/td (rb/get "category" c)))))

(define (rb/step-list r)
  "Unordered list of a recipe's steps."
  (rb/ul (rb/map rb/step-item (rb/get "steps" r))))

(define (rb/step-item s)
  "List item for a single step with duration and note."
  (let ((name (rb/get "name" s))
        (dur (rb/get "duration" s))
        (note (rb/get "note" s)))
    (rb/li
      (string-append
        (string-append name " (" (rb/qty-str dur) ")")
        (if (null? note) "" (string-append " - " (as-str note)))))))

(define (rb/package-rows r)
  "Header plus one row per package option."
  (let ((pkgs (rb/get "packages" r)))
    (cons
      (rb/tr (list (rb/th "Package") (rb/th "Units/batch") (rb/th "L/unit") (rb/th "Price") (rb/th "Margin/unit")))
      (rb/map (lambda (pk) (rb/package-row r pk)) pkgs))))

(define (rb/package-row r pk)
  "HTML row for one package option and its margins."
  (let ((biz (rb/business r pk (rb/fixed-costs))))
    (rb/tr (list
      (rb/td (rb/get "product_name" pk))
      (rb/td (as-str (rb/get "units_per_batch" pk)))
      (rb/td (rb/qty-str (rb/get "liters_per_unit" pk)))
      (rb/money-cell (rb/get "selling_price" pk))
      (rb/money-cell (rb/contribution-margin-per-unit biz))))))

(define (rb/report-table r)
  "Monthly report as a metric/value table."
  (let ((pkgs (rb/get "packages" r)))
    (if (null? pkgs)
        (rb/p "No packages configured")
      (let ((report (cdr (rb/calc-one (rb/get "name" r)))))
        (rb/table
          (list
            (rb/tr (list (rb/th "Metric") (rb/th "Value")))
            (rb/tr (list (rb/td "Units/month") (rb/td (as-str (rb/get "units_per_month" report)))))
            (rb/tr (list (rb/td "Revenue/month") (rb/money-cell (rb/get "revenue_per_month" report))))
            (rb/tr (list (rb/td "Variable cost/batch") (rb/money-cell (rb/get "variable_cost_per_batch" report))))
            (rb/tr (list (rb/td "Variable cost/unit") (rb/money-cell (rb/get "variable_cost_per_unit" report))))
            (rb/tr (list (rb/td "Fixed cost/month") (rb/money-cell (rb/get "fixed_cost_per_month" report))))
            (rb/tr (list (rb/td "Total cost/month") (rb/money-cell (rb/get "total_cost_per_month" report))))
            (rb/tr (list (rb/td "Profit/month") (rb/money-cell (rb/get "profit_per_month" report))))
            (rb/tr (list (rb/td "Margin/unit") (rb/money-cell (rb/get "contribution_margin_per_unit" report))))
            (rb/tr (list (rb/td "Breakeven units") (rb/td (as-str (rb/get "breakeven_units_per_month" report)))))))))))

;; ============================================================
;; SHOPPING LIST PAGE — /shopping?name=...
;; ============================================================

(define (rb/page-shopping)
  "Shopping-list page route handler."
  (seq
    (let ((name (request-param "name")))
      (if (null? name)
          (html/page (list (rb/h1 "Shopping list not found")))
        (let ((r (rb/find-recipe (as-str name))))
          (if (null? r)
              (html/page (list (rb/h1 "Recipe not found")))
            (html/page
              (list
                (rb/h1 (string-append "Shopping List: " (rb/get "name" r)))
                (rb/shopping-body r)))))))))

(define (rb/shopping-body r)
  "Shopping list table for one recipe."
  (let ((costs (rb/get "costs" r)))
    (rb/table
      (cons
        (rb/tr (list (rb/th "Ingredient") (rb/th "Order qty") (rb/th "Price") (rb/th "Source")))
        (rb/map rb/shopping-row costs)))))

(define (rb/shopping-row c)
  "HTML row for one shopping-list line."
  (let ((src (rb/get "source" c)))
    (rb/tr (list
      (rb/td (rb/get "name" c))
      (rb/td (rb/qty-str (rb/get "order_qty" c)))
      (rb/money-cell (rb/get "order_price" c))
      (rb/td (if (null? src) "" (as-str src)))))))

;; ============================================================
;; CAPITAL PAGE — /capital
;; ============================================================

(define (rb/page-capital)
  "Capital plan page route handler."
  (seq
    (html/page
      (list
        (rb/h1 "Capital Plan")
        (rb/h2 "Equipment")
        (rb/equipment-table)
        (rb/h2 "Fixed Costs")
        (rb/fixed-cost-table)))))

(define (rb/equipment-table)
  "Equipment table with impact/unlocks columns."
  (rb/table
    (cons
      (rb/tr (list (rb/th "Priority") (rb/th "Item") (rb/th "Price") (rb/th "Impact") (rb/th "Unlocks")))
      (rb/map rb/equipment-row (rb/equipment-list)))))

(define (rb/equipment-row e)
  "HTML row for one equipment entry."
  (rb/tr (list
    (rb/td (as-str (rb/get "priority" e)))
    (rb/td (rb/get "name" e))
    (rb/money-cell (rb/get "price" e))
    (rb/td (let ((i (rb/get "impact" e))) (if (null? i) "" (as-str i))))
    (rb/td (let ((u (rb/get "unlocks" e))) (if (null? u) "" (as-str u)))))))

(define (rb/fixed-cost-table)
  "Fixed-cost summary table."
  (rb/table
    (cons
      (rb/tr (list (rb/th "Name") (rb/th "Monthly cost")))
      (rb/map rb/fixed-cost-row (rb/fixed-costs)))))

(define (rb/fixed-cost-row f)
  "HTML row for one fixed-cost entry."
  (rb/tr (list
    (rb/td (rb/get "name" f))
    (rb/money-cell (rb/get "monthly_cost" f)))))

;; ============================================================
;; LABEL PAGE — /label?name=...
;; ============================================================

(define (rb/page-label)
  "Label page route handler (renders a print label)."
  (seq
    (let ((name (request-param "name")))
      (if (null? name)
          (html/page (list (rb/h1 "Label not found")))
        (let ((r (rb/find-recipe (as-str name))))
          (if (null? r)
              (html/page (list (rb/h1 "Recipe not found")))
            (html/page
              (list
                (render-html
                  (string-append
                    "<p><a href=\"/labels?name=" (url-encode (as-str name))
                    "\">Print 4-up sheet (Avery 94252) →</a></p>"))
                (render-html (rb/label-html r))))))))))

(define (rb/label-html r)
  "Inline HTML for a printable product label."
  (let ((name (rb/get "name" r)))
    (string-append
      "<div style=\"width:4in;height:3in;border:2px solid #8B4513;border-radius:8px;padding:12px;font-family:serif;background:linear-gradient(135deg,#f5e6d3,#f0d5b8);display:flex;flex-direction:column;justify-content:space-between\">"
      "<div style=\"text-align:center\">"
      "<h1 style=\"font-size:18px;margin:4px 0;color:#5C3317\">" name "</h1>"
      "<p style=\"font-size:10px;margin:2px 0;color:#8B4513;font-style:italic\">Ginger Bug Soda - Handcrafted in Wyoming</p>"
      "</div>"
      "<div style=\"text-align:center\">"
      "<p style=\"font-size:9px;margin:2px 0;color:#5C3317\">Made with fresh ginger bug starter</p>"
      "<p style=\"font-size:9px;margin:2px 0;color:#5C3317\">Keep refrigerated - Live culture</p>"
      "</div>"
      "<div style=\"text-align:center\">"
      "<p style=\"font-size:8px;margin:2px 0;color:#8B4513\">Not licensed or inspected per Wyo. Stat. 35-10-301</p>"
      "<p style=\"font-size:8px;margin:2px 0;color:#8B4513\">Pricklypear Soda Co.</p>"
      "</div>"
      "</div>")))

;; ============================================================
;; LABEL SHEET — /labels — Avery Presta 94252 (4-up, 3in x 4in)
;; Fills one letter sheet with 4 copies of the label, laid out for
;; Avery 94252 die cuts: 2 cols x 2 rows, 3in x 4in cells, 1.25in
;; side margins, 1.5in top margin (kept clear of the printer's
;; unprintable band so full-bleed backgrounds survive).
;;
;; First print with any new stock: add &cal=1 — the cells print as
;; dashed red boxes so you can hold the sheet to the light against
;; the die cuts. Nudge with query params (persisted in the URL):
;;   &x0=1.25in &y0=1.5in &w=3in &h=4in &cols=2 &rows=2 &sheets=1
;; ============================================================

(define (rb/label-cells names n)
  "First n label names, cycling names until filled."
  (if (<= n 0)
      ()
      (if (null? names)
          ()
          (cons (car names)
                (rb/label-cells (append (cdr names) (list (car names))) (- n 1))))))

(define (rb/label-cell name batch cal?)
  "One 3in x 4in label cell (dashed calibration box when cal?)."
  (string-append
    "<div class=\"lbl"
    (if cal? " lbl-cal" "")
    "\"><div class=\"lbl-top\"><div class=\"lbl-name\">"
    (html-escape name)
    "</div><div class=\"lbl-tag\">Ginger bug soda — handcrafted in Wyoming</div></div>"
    (if cal?
        ""
        (string-append
          "<div class=\"lbl-body\">Brewed from real ginger bug starter — "
          "never artificial flavoring.<br>Ingredients: filtered water, cane sugar, ginger, lemon, live cultures.<br> "
          "Refrigerate. Keep the culture alive.<br>"
          (if (string-eq batch "")
              ""
              (string-append "Batch #" (html-escape batch) "<br>"))
          "</div>"))
    "<div class=\"lbl-law\">Not licensed or inspected per Wyo. Stat. 35-10-301<br>Pricklypear Soda Co.</div></div>"))

(define (rb/label-sheet-html names cols rows w h x0 y0 sheets batch cal?)
  "One or more letter sheets of 4-up labels, print-ready at 100% scale."
  (string-append
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Label sheet — Avery 94252</title>"
    "<style>"
    "@page{size:letter portrait;margin:0}"
    "*{box-sizing:border-box;-webkit-print-color-adjust:exact;print-color-adjust:exact}"
    "body{margin:0;background:#8888;}"
    ".ppsheet{display:grid;grid-template-columns:repeat(" (str cols) "," w ");"
    "grid-auto-rows:" h ";padding:" y0 " " x0 ";background:#fff;}"
    ".lbl{width:" w ";height:" h ";border:2px solid #8B4513;border-radius:12px;"
    "background:linear-gradient(160deg,#f7ecd9,#eed7b4);color:#5C3317;"
    "display:flex;flex-direction:column;justify-content:space-between;"
    "padding:18px 20px;font-family:Georgia,'Times New Roman',serif;}"
    ".lbl-cal{border:2px dashed #d00;border-radius:12px;background:transparent;}"
    ".lbl-name{font-size:20pt;font-weight:700;letter-spacing:1px;text-align:center;"
    "margin:6px 0 2px;text-transform:uppercase;}"
    ".lbl-tag{font-size:9pt;font-style:italic;text-align:center;color:#8B4513;}"
    ".lbl-rule{border-top:1px solid #B08D5F;margin:8px 24px;}"
    ".lbl-body{font-size:9pt;text-align:center;line-height:1.6;}"
    ".lbl-law{font-size:6.5pt;text-align:center;color:#8B4513;line-height:1.4;}"
    ".ppbar{font-family:monospace;font-size:12px;padding:10px 14px;background:#222;color:#eee;}"
    ".ppbar b{color:#ffd;} .ppbar button{font-size:14px;padding:6px 18px;cursor:pointer;}"
    "@media print{.ppbar{display:none} .ppsheet{box-shadow:none} body{background:#fff}"
    ".ppsheet{page-break-after:always;} .ppsheet:last-child{page-break-after:auto;}}"
    "</style></head><body>"
    "<div class=\"ppbar\">Avery Presta 94252 — 4-up 3&times;4in — print dialog: <b>Margins: None · Scale: 100%</b> · Background graphics: <b>ON</b>. "
    "Calibration: <b>&cal=1</b> · nudge: <b>&x0= &y0= &w= &h= &cols= &rows= &sheets=</b> · "
    "<button onclick=\"window.print()\">Print</button></div>"
    (string-join
      (rb/map (lambda (ig)
        (string-append
          "<div class=\"ppsheet\">"
          (string-join (rb/map (lambda (n) (rb/label-cell (string-trim n) batch cal?)) (rb/label-cells names (* cols rows))) "")
          "</div>"))
        (rb/label-cells (list "x") sheets))
      "")
    "</body></html>"))

(define (rb/param-default name default)
  "Request param name as trimmed string, or default when missing/empty."
  (let ((s (request-param name)))
    (if (null? s)
        default
        (if (string-eq (string-trim s) "") default (string-trim s)))))

(define (rb/page-labels)
  "Label sheet page (Avery 94252 4-up): fills a sheet with copies of the label."
  (seq
    (let ((all (rb/param-default "names" ""))
          (one (rb/param-default "name" "Traditional Root Beer")))
      (let ((names (if (string-eq all "") (list one) (string-split all ","))))
        (let ((cols (parse-int (rb/param-default "cols" "2")))
              (rows (parse-int (rb/param-default "rows" "2")))
              (sheets (parse-int (rb/param-default "sheets" "1")))
              (batch (rb/param-default "batch" ""))
              (cal (rb/param-default "cal" "")))
          (bytes-response "text/html" "0"
            (rb/label-sheet-html
              names cols rows
              (rb/param-default "w" "3in")
              (rb/param-default "h" "4in")
              (rb/param-default "x0" "1.25in")
              (rb/param-default "y0" "1.5in")
              sheets batch
              (not (string-eq cal "")))))))))

;; ============================================================
;; BATCHES PAGE — /batches
;; ============================================================

(define (rb/page-batches)
  "Batch log page route handler (table + log-a-brew form; POST logs a brew)."
  (seq
    (if (string-eq (request-method) "POST")
        (rb/brew-log-post)
      (seq
        (rb/seed-brews!)
        (html/page
          (list
            (rb/h1 "Batch Log")
            (rb/batch-table)
            (rb/h2 "Log a brew")
            (html/div (html/attr "id" "brew-status") "")
            (rb/brew-form)))))))

(define (rb/batch-table)
  "Batch log table from persisted brew rows (historical log seeded in)."
  (let ((rws (rb/brew-rows)))
    (if (null? rws)
        (rb/p "No batches logged yet.")
      (rb/table
        (cons
          (rb/tr (list (rb/th "Date") (rb/th "Recipe") (rb/th "Yield") (rb/th "Packaged") (rb/th "Ferment") (rb/th "Rating") (rb/th "Notes")))
          (rb/map rb/batch-row rws))))))

(define (rb/batch-row row)
  "HTML row for one persisted brew row."
  (let ((f (cdr row)))
    (rb/tr (list
      (rb/td (dict-get f "date"))
      (rb/td (dict-get f "recipe"))
      (rb/td (string-append (dict-get f "yield_liters") " L"))
      (rb/td (dict-get f "packaged"))
      (rb/td (dict-get f "ferment_days"))
      (rb/td (dict-get f "rating"))
      (rb/td (dict-get f "notes"))))))

(define (rb/brew-form)
  "HTML form to log a new brew (htmx POST to /batches)."
  (html/form
    "POST"
    "/batches"
    (html/attrs
      (list
        (html/attr "hx-post" "/batches")
        (html/attr "hx-target" "#brew-status")
        (html/attr "hx-swap" "innerHTML")
        (html/attr "hx-on::after-request" "if(event.detail.successful){this.reset()}")))
    (html/join
      (list
        (html/label (html/attr "for" "brew-date") "Date")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "date")
              (html/attr "id" "brew-date")
              (html/attr "name" "date")
              (html/attr "required" "required"))))
        (html/label (html/attr "for" "brew-recipe") "Recipe")
        (html/select
          "recipe"
          (html/attrs
            (list
              (html/attr "id" "brew-recipe")
              (html/attr "required" "required")))
          (html/join
            (rb/map
              (lambda (pair) (html/option (car pair) (car pair) #f))
              (rb/all-recipes))))
        (html/label (html/attr "for" "brew-yield") "Yield (L)")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "number")
              (html/attr "id" "brew-yield")
              (html/attr "name" "yield_liters")
              (html/attr "step" "0.1")
              (html/attr "required" "required"))))
        (html/label (html/attr "for" "brew-pkg-name") "Packaged into")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "text")
              (html/attr "id" "brew-pkg-name")
              (html/attr "name" "packaged_name")
              (html/attr "placeholder" "500mL PET")
              (html/attr "required" "required"))))
        (html/label (html/attr "for" "brew-pkg-count") "Units")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "number")
              (html/attr "id" "brew-pkg-count")
              (html/attr "name" "packaged_count")
              (html/attr "min" "0")
              (html/attr "required" "required"))))
        (html/label (html/attr "for" "brew-ferment") "Ferment (days)")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "number")
              (html/attr "id" "brew-ferment")
              (html/attr "name" "ferment_days")
              (html/attr "min" "0"))))
        (html/label (html/attr "for" "brew-carb") "Carbonation")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "text")
              (html/attr "id" "brew-carb")
              (html/attr "name" "carbonation")
              (html/attr "placeholder" "light / medium (optional)"))))
        (html/label (html/attr "for" "brew-rating") "Rating (0-10)")
        (html/input
          (html/attrs
            (list
              (html/attr "type" "number")
              (html/attr "id" "brew-rating")
              (html/attr "name" "rating")
              (html/attr "min" "0")
              (html/attr "max" "10"))))
        (html/label (html/attr "for" "brew-notes") "Notes")
        (html/textarea
          "notes"
          (html/attrs
            (list
              (html/attr "id" "brew-notes")
              (html/attr "rows" "4")
              (html/attr "placeholder" "Tasting notes, what to change next time")))
          "")
        (html/div
          (html/attr "class" "capture-row")
          (html/button (html/attr "type" "submit") "Log brew"))))))

(define (rb/brew-status ok msg)
  "Status line for the brew form, ok/err classed."
  (html/p* (html/attr "class" (if ok "ok" "err")) msg))

(define (rb/param-str p)
  "Coerce a request param to a trimmed string; () stays ()."
  (if (null? p)
      ()
    (string-trim (as-str p))))

(define (rb/param-or p default)
  "Param as trimmed string, or the default when missing/empty."
  (let ((s (rb/param-str p)))
    (if (null? s) default (if (string-eq s "") default s))))

(define (rb/brew-log-post)
  "Handle POST /batches: validate, save the brew, redirect or render status."
  (let ((date (rb/param-str (request-param "date")))
        (recipe (rb/param-str (request-param "recipe"))))
    (if (or (null? date) (null? recipe) (string-eq date "") (string-eq recipe ""))
        (if (hx-request?)
            (render-html (rb/brew-status #f "Date and recipe are required."))
          (redirect "/batches"))
      (seq
        (rb/save-brew!
          (rb/make-brew
            date
            recipe
            (rb/param-or (request-param "yield_liters") "0")
            (list (packaged
                    (rb/param-or (request-param "packaged_name") "bottles")
                    (rb/param-or (request-param "packaged_count") "0")))
            (rb/param-or (request-param "ferment_days") "")
            (rb/param-or (request-param "carbonation") "")
            (rb/param-or (request-param "rating") "")
            (rb/param-or (request-param "notes") "")))
        (if (hx-request?)
            (render-html (rb/brew-status #t "Brew logged."))
          (redirect "/batches"))))))

;; ============================================================
;; PURCHASES PAGE — /purchases
;; ============================================================

(define (rb/page-purchases)
  "Purchase log page route handler."
  (seq
    (html/page
      (list
        (rb/h1 "Purchase Log")
        (rb/purchase-table)))))

(define (rb/purchase-table)
  "Purchase log table."
  (rb/table
    (cons
      (rb/tr (list (rb/th "Date") (rb/th "Item") (rb/th "Qty") (rb/th "Price") (rb/th "Source")))
      (rb/map rb/purchase-row (rb/purchases)))))

(define (rb/purchase-row p)
  "HTML row for one purchase entry."
  (let ((src (rb/get "source" p)))
    (rb/tr (list
      (rb/td (rb/get "date" p))
      (rb/td (rb/get "name" p))
      (rb/td (rb/qty-str (rb/get "qty" p)))
      (rb/money-cell (rb/get "price" p))
      (rb/td (if (null? src) "" (as-str src)))))))

;; ============================================================
;; ORDERS — record actual sales (closes planned→actual loop)
;; ============================================================

(define (order customer date line-items opts)
  "Build an order alist."
  (cons (cons "type" "order")
  (cons (cons "customer" customer)
  (cons (cons "date" date)
  (cons (cons "line_items" line-items) opts)))))

(define (line-item recipe-name qty price)
  "Build an order line item alist."
  (list
    (cons "recipe" recipe-name)
    (cons "qty" qty)
    (cons "price" price)))

;; seed orders (empty — no historical orders in OCaml rb)
(define rb/orders (list))

;; order persistence
(define (rb/init-order-storage!)
  "Ensure the order type exists."
  (if (null? (type-fields (quote order)))
      (deftype (quote order) (quote customer) (quote date) (quote line_items) (quote total) (quote created_day))
    ()))

(define (rb/save-order! ord)
  "Persist an order alist to the tree."
  (seq
    (rb/init-order-storage!)
    (create (quote order)
      (dict-set
        (dict-set
          (dict-set
            (dict-set
              (dict-set (json-emit "{}")
                "customer" (rb/get "customer" ord))
              "date" (rb/get "date" ord))
            "line_items" (json-emit (rb/get "line_items" ord)))
          "total" (as-str (rb/order-total ord)))
        "created_day" (date-today)))))

(define (rb/order-total ord)
  "Total revenue of an order's line items."
  (rb/sum (lambda (li) (* (rb/get "qty" li) (rb/get "price" li)))
          (rb/get "line_items" ord)))

(define (rb/order-rows)
  "All persisted order rows."
  (rows (quote order)))

;; ============================================================
;; PLANNED VS ACTUAL
;; ============================================================

;; Compare planned profit (from calc) vs actual revenue (from orders).
;; Returns ((planned_revenue . X) (actual_revenue . Y) (variance . Z))
(define (rb/planned-vs-actual)
  "Planned vs actual revenue with variance."
  (let ((planned (rb/sum (lambda (p) (rb/get "revenue_per_month" (cdr p))) (rb/calc-all))))
    (let ((actual (rb/actual-revenue)))
      (list
        (cons "planned_revenue" planned)
        (cons "actual_revenue" actual)
        (cons "variance" (- actual planned))))))

(define (rb/actual-revenue)
  "Sum of totals across all order rows."
  (rb/sum (lambda (row) (parse-float (dict-get (cdr row) "total"))) (rb/order-rows)))

;; ============================================================
;; ORDERS PAGE — /orders
;; ============================================================

(define (rb/page-orders)
  "Orders page route handler (table + planned vs actual)."
  (seq
    (html/page
      (list
        (rb/h1 "Orders")
        (rb/order-table)
        (rb/h2 "Planned vs Actual")
        (rb/pva-table)))))

(define (rb/order-table)
  "Orders table, or a note when none exist."
  (let ((rws (rb/order-rows)))
    (if (null? rws)
        (rb/p "No orders yet. Sell some soda!")
      (rb/table
        (cons
          (rb/tr (list (rb/th "Date") (rb/th "Customer") (rb/th "Total")))
          (rb/map rb/order-data-row rws))))))

(define (rb/order-data-row row)
  "HTML row for one persisted order."
  (let ((f (cdr row)))
    (rb/tr (list
      (rb/td (dict-get f "date"))
      (rb/td (dict-get f "customer"))
      (rb/money-cell (parse-float (dict-get f "total")))))))

(define (rb/pva-table)
  "Planned-vs-actual metric table."
  (let ((pva (rb/planned-vs-actual)))
    (rb/table
      (list
        (rb/tr (list (rb/th "Metric") (rb/th "Value")))
        (rb/tr (list (rb/td "Planned revenue/month") (rb/money-cell (rb/get "planned_revenue" pva))))
        (rb/tr (list (rb/td "Actual revenue (all orders)") (rb/money-cell (rb/get "actual_revenue" pva))))
        (rb/tr (list (rb/td "Variance") (rb/money-cell (rb/get "variance" pva))))))))

;; ============================================================
;; REPORT PAGE — /report (multi-recipe business overview)
;; ============================================================

(define (rb/page-report)
  "Business report page route handler."
  (seq
    (html/page
      (list
        (rb/h1 "Business Report")
        (rb/h2 "Monthly Profit by Recipe")
        (rb/report-summary-table)
        (rb/h2 "Plan")
        (rb/plan-table)))))

(define (rb/report-summary-table)
  "Monthly profit-by-recipe summary table."
  (rb/table
    (cons
      (rb/tr (list (rb/th "Recipe") (rb/th "Revenue/mo") (rb/th "Cost/mo") (rb/th "Profit/mo") (rb/th "Margin/unit")))
      (rb/map rb/report-summary-row (rb/calc-all)))))

(define (rb/report-summary-row pair)
  "HTML row for one recipe's report summary."
  (let ((name (car pair)) (report (cdr pair)))
    (rb/tr (list
      (rb/td (rb/link (string-append "/recipe?name=" (html-escape name)) name))
      (rb/money-cell (rb/get "revenue_per_month" report))
      (rb/money-cell (rb/get "total_cost_per_month" report))
      (rb/money-cell (rb/get "profit_per_month" report))
      (rb/money-cell (rb/get "contribution_margin_per_unit" report))))))

(define (rb/plan-table)
  "Plan configuration summary table."
  (rb/table
    (list
      (rb/tr (list (rb/th "Metric") (rb/th "Value")))
      (rb/tr (list (rb/td "Batches/month") (rb/td (as-str (rb/get "batches_per_month" (rb/plan-config))))))
      (rb/tr (list (rb/td "Starter recipe") (rb/td (rb/get "starter_recipe" (rb/plan-config))))))))

;; ============================================================
;; ROUTES
;; ============================================================

(route "/recipes" "rb/page-recipes")
(route "/recipe" "rb/page-recipe-detail")
(route "/shopping" "rb/page-shopping")
(route "/capital" "rb/page-capital")
(route "/label" "rb/page-label")
(route "/labels" "rb/page-labels")
(route "/batches" "rb/page-batches")
(route "/purchases" "rb/page-purchases")
(route "/orders" "rb/page-orders")
(route "/report" "rb/page-report")
