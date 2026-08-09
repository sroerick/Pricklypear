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
  (let ((pair (assoc key alist)))
    (if (null? pair) () (cdr pair))))

(define (rb/has? key alist)
  (not (null? (assoc key alist))))

(define (rb/set key val alist)
  (cons (cons key val) alist))

(define (rb/append a b)
  (if (null? a) b (cons (car a) (rb/append (cdr a) b))))

(define (rb/map fn xs)
  (if (null? xs) (list)
    (cons (fn (car xs)) (rb/map fn (cdr xs)))))

(define (rb/filter pred xs)
  (if (null? xs) (list)
    (if (pred (car xs))
        (cons (car xs) (rb/filter pred (cdr xs)))
      (rb/filter pred (cdr xs)))))

(define (rb/sum fn xs)
  (if (null? xs) 0.0 (+ (fn (car xs)) (rb/sum fn (cdr xs)))))

;; count that works on cons-built lists (not just VLists)
(define (rb/count xs)
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

(define (rb/unit-dim u)   (car (rb/get u rb/unit-table)))
(define (rb/unit-factor u) (cdr (rb/get u rb/unit-table)))

;; quantity: (value . "unit")
(define (qty val unit) (cons val unit))
(define (qty-val q) (car q))
(define (qty-unit q) (cdr q))

(define (rb/to-base q)
  (* (qty-val q) (rb/unit-factor (qty-unit q))))

(define (rb/compatible? q1 q2)
  (string-eq (rb/unit-dim (qty-unit q1)) (rb/unit-dim (qty-unit q2))))

(define (rb/convert q target-unit)
  (if (not (string-eq (rb/unit-dim (qty-unit q)) (rb/unit-dim target-unit)))
      q
    (let ((base (rb/to-base q)))
      (qty (/ base (rb/unit-factor target-unit)) target-unit))))

(define (rb/qty-str q)
  (string-append (as-str (qty-val q)) " " (qty-unit q)))

;; ============================================================
;; COSTS (ingredients, utilities, packaging)
;; A cost is an alist with type/name/qty_per_batch/order_qty/order_price/category
;; plus optional source/url/note/on_hand.
;; ============================================================

(define (ingredient name qbatch oqty price opts)
  (rb/make-cost name qbatch oqty price "ingredient" opts))

(define (utility name qbatch oqty price opts)
  (rb/make-cost name qbatch oqty price "utility" opts))

(define (pkg-cost name qbatch oqty price opts)
  (rb/make-cost name qbatch oqty price "packaging" opts))

(define (rb/make-cost name qbatch oqty price category opts)
  (cons (cons "type" "cost")
  (cons (cons "name" name)
  (cons (cons "qty_per_batch" qbatch)
  (cons (cons "order_qty" oqty)
  (cons (cons "order_price" price)
  (cons (cons "category" category) opts)))))))

;; opt constructors — each returns (key . value) for the opts alist
(define (source s) (cons "source" s))
(define (url s) (cons "url" s))
(define (note s) (cons "note" s))
(define (on-hand q) (cons "on_hand" q))

;; ============================================================
;; STEPS
;; ============================================================

(define (step name duration opts)
  (cons (cons "type" "step")
  (cons (cons "name" name)
  (cons (cons "duration" duration) opts))))

(define (use name) (cons "use" name))

;; ============================================================
;; PACKAGES
;; A package is an alist with product info + packaging_costs list.
;; ============================================================

(define (package product-name units-per-batch liters-per-unit selling-price pkgs)
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
  (list
    (cons "type" "fixed")
    (cons "name" name)
    (cons "monthly_cost" monthly)))

;; equipment: capital purchase with priority tier
(define (equipment name price priority opts)
  (cons (cons "type" "equipment")
  (cons (cons "name" name)
  (cons (cons "price" price)
  (cons (cons "priority" priority) opts)))))

(define (impact s) (cons "impact" s))
(define (unlocks s) (cons "unlocks" s))

(define (plan batches-per-month starter)
  (list
    (cons "batches_per_month" batches-per-month)
    (cons "starter_recipe" starter)))

;; ============================================================
;; SHARED BLOCKS & RECIPE IMPORTS
;; ============================================================

;; A shared block is a named list of costs, reusable across recipes.
(define (shared name costs)
  (list
    (cons "type" "shared")
    (cons "name" name)
    (cons "costs" costs)))

;; uses: recipe-level import of a shared block. Returns a marker that
;; rb/collect-recipe splices — the block's costs get appended to the recipe.
(define (uses block)
  (list
    (cons "type" "uses")
    (cons "block" block)))

;; ============================================================
;; MIXED RUNS — allocate batch liters across package types
;; ============================================================

;; A mix-part is (package-name . count)
(define (mixed name parts)
  (list
    (cons "type" "mixed")
    (cons "name" name)
    (cons "parts" parts)))

;; ============================================================
;; RECIPES — collect items into a recipe alist
;; ============================================================

(define (recipe name batch-liters items)
  (rb/collect-recipe
    (list (cons "name" name) (cons "batch_liters" batch-liters))
    items))

(define (rb/collect-recipe base items)
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
  (let ((oqty-base (rb/to-base (rb/get "order_qty" c))))
    (/ (rb/get "order_price" c) oqty-base)))

;; line_cost = to_base(qty_per_batch) * unit_cost
(define (rb/line-cost c)
  (* (rb/to-base (rb/get "qty_per_batch" c)) (rb/unit-cost c)))

;; total variable cost = sum of line costs
(define (rb/variable-cost-total costs)
  (rb/sum rb/line-cost costs))

;; fixed cost total = sum of monthly_cost
(define (rb/fixed-cost-total costs)
  (rb/sum (lambda (c) (rb/get "monthly_cost" c)) costs))

;; filter costs by category
(define (rb/costs-by-category cat costs)
  (rb/filter (lambda (c) (string-eq (rb/get "category" c) cat)) costs))

(define (rb/ingredient-costs costs)
  (rb/costs-by-category "ingredient" costs))

(define (rb/packaging-costs costs)
  (rb/costs-by-category "packaging" costs))

;; ============================================================
;; BUSINESS (recipe + package option → business alist)
;; ============================================================

(define (rb/business recipe-data pkg fixed-costs)
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
  (let ((units (rb/get "units_per_batch" biz)))
    (/ (rb/variable-cost-total (rb/get "variable_costs" biz)) units)))

(define (rb/contribution-margin-per-unit biz)
  (- (rb/get "selling_price" biz) (rb/variable-cost-per-unit biz)))

;; ============================================================
;; MONTHLY REPORT
;; ============================================================

(define (rb/monthly-report biz pln)
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
  (if (null? (cdr xs))
      (car xs)
    (let ((rest (rb/min-list (cdr xs))))
      (if (< (car xs) rest) (car xs) rest))))

(define (rb/find-limiting costs scale)
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
  (rb/make-brew date recipe-name yield-liters packaged-list ferment-days carbonation rating notes))

(define (rb/make-brew date recipe-name yield-liters packaged-list ferment-days carbonation rating notes)
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
(define (packaged pkg-name count) (cons pkg-name count))

;; A purchase log: records an actual ingredient/equipment buy
(define (bought name qty price date opts)
  (cons (cons "type" "purchase")
  (cons (cons "name" name)
  (cons (cons "qty" qty)
  (cons (cons "price" price)
  (cons (cons "date" date) opts))))))

;; ============================================================
;; PERSISTENCE — save recipe to the tree
;; ============================================================

(define (rb/init-storage!)
  (if (null? (type-fields (quote recipe)))
      (deftype (quote recipe) (quote name) (quote batch_liters) (quote source) (quote created_day))
    ()))

(define (rb/save-recipe! recipe-data source-text)
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
  (rows (quote recipe)))

(define (rb/fetch-recipe id)
  (get (quote recipe) id))

(define (rb/seed-all!)
  (seq
    (rb/init-storage!)
    (rb/seed-recipes rb/all-recipes)))

(define (rb/seed-recipes recipes)
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
  (rb/find-in-rows name (rows (quote recipe))))

(define (rb/find-in-rows name rws)
  (if (null? rws)
      ()
    (if (string-eq (dict-get (cdr (car rws)) "name") name)
        (car rws)
      (rb/find-in-rows name (cdr rws)))))
;; libs/rb/recipes.pp — all rb recipes converted to Nopales
;; Hand-converted from /home/roerick/dev/wyo.tech/rb/data/*.rb

;; ============================================================
;; SHARED BLOCKS
;; ============================================================

(define rb/ginger-bug-base
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

(define rb/simple-root-beer
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

(define rb/root-beer
  (recipe "Ginger Bug Root Beer" (qty 10 "L")
    (list
      (uses rb/ginger-bug-base)
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

(define rb/ginger-ale
  (recipe "Ginger Ale" (qty 10 "L")
    (list
      (uses rb/ginger-bug-base)
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

(define rb/lemonade
  (recipe "Ginger Bug Lemonade" (qty 10 "L")
    (list
      (uses rb/ginger-bug-base)
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

(define rb/cola
  (recipe "Ginger Bug Cola" (qty 10 "L")
    (list
      (uses rb/ginger-bug-base)
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

(define rb/happy-ginger
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

(define rb/traditional-root-beer
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

(define rb/experimental-004
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

(define rb/fixed-costs
  (list
    (fixed-cost "Booth rent" 60.00)))

(define rb/equipment-list
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

(define rb/plan-config
  (plan 4 "Ginger Bug Root Beer"))

;; ============================================================
;; OPERATING LOG (from batch_logs/ and orders/)
;; ============================================================

(define rb/batches
  (list
    (brew "2026-06-05" "Ginger Ale" 3.16 (list (packaged "500mL PET" 8)) 3 "light" 6
      "Ginger bug was weak, carbonation suffered. Too sweet (145 g/L). Lemon+vanilla combo was iffy. Some said 'tastes like Chinese food but in the best way.' Next time: reduce ginger to 180g, cut sugar, swap lemon for lime or pineapple.")
    (brew "2026-06-12" "Simple Root Beer" 6.0 (list (packaged "500mL" 12)) 0 () 0
      "First batch. Sassafras + wintergreen + vanilla extract. No molasses, no sarsaparilla. Old-school root beer profile.")
    (brew "2026-07-11" "Happy Ginger Ginger Beer" 12.0 (list (packaged "500mL PET" 24)) 3 () 9
      "Fresh+spent ginger hypothesis confirmed - clean ginger heat, no 'Chinese food' note from brew 1. Lime and pineapple are subtle. Sugar at 105 g/L landed well. Yield 12L vs 9.44L planned (27% over). BACKSWEETENED at bottling - load-bearing for 9/10 result.")
    (brew "2026-07-12" "Experimental Root Beer 004: 1896 edition" 6.0 (list (packaged "500mL PET" 12)) 7 () 7
      "Brewed 2026-07-05 (kitchen-sink herb haul); bottled ~2026-07-12. Molasses later diagnosed as 100% the off-flavor (dropped in Traditional Root Beer v2). Selling as Experimental 004 / 1896 edition.")))

(define rb/purchases
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

(define rb/all-recipes
  (list
    (cons "Simple Root Beer" rb/simple-root-beer)
    (cons "Ginger Bug Root Beer" rb/root-beer)
    (cons "Ginger Ale" rb/ginger-ale)
    (cons "Ginger Bug Lemonade" rb/lemonade)
    (cons "Ginger Bug Cola" rb/cola)
    (cons "Happy Ginger Ginger Beer" rb/happy-ginger)
    (cons "Traditional Root Beer" rb/traditional-root-beer)
    (cons "Experimental Root Beer 004: 1896 edition" rb/experimental-004)))

;; lookup recipe by name
(define (rb/find-recipe name)
  (rb/get name rb/all-recipes))

;; calc one recipe with first package option + fixed costs
;; returns (name . report) or (name . error-string)
(define (rb/calc-one name)
  (let ((r (rb/find-recipe name)))
    (if (null? r)
        (cons name "not found")
      (let ((pkgs (rb/get "packages" r)))
        (if (null? pkgs)
            (cons name "no packages")
          (let ((biz (rb/business r (car pkgs) rb/fixed-costs)))
            (cons name (rb/monthly-report biz rb/plan-config))))))))

;; calc all recipes -> list of (name . report)
(define (rb/calc-all)
  (rb/map (lambda (pair) (rb/calc-one (car pair))) rb/all-recipes))
;; libs/rb/ui.pp — rb route handlers (htmx, server-rendered)
;; Depends on: libs/rb/lib.pp, libs/rb/recipes.pp, html package

;; ============================================================
;; HTML HELPERS
;; ============================================================

(define (rb/h1 s) (html/h1 s))
(define (rb/h2 s) (html/h2 s))
(define (rb/h3 s) (html/h3 s))
(define (rb/p s) (html/tag "p" "" s))
(define (rb/td s) (html/tag "td" "" s))
(define (rb/th s) (html/tag "th" "" s))

(define (rb/tr cells) (html/tag "tr" "" (html/join cells)))
(define (rb/table rows) (html/tag "table" (html/attr "class" "rb-table") (html/join rows)))

(define (rb/li s) (html/tag "li" "" s))
(define (rb/ul items) (html/tag "ul" "" (html/join items)))

(define (rb/card title body)
  (html/div (html/attr "class" "card")
    (html/join (list (html/h3 title) body))))

(define (rb/link href text)
  (html/tag "a" (html/attr "href" href) text))

(define (rb/money-cell n)
  (rb/td (rb/money n)))

;; ============================================================
;; RECIPE LIST PAGE — /recipes
;; ============================================================

(define (rb/page-recipes)
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Recipes")
        (rb/table
          (rb/map rb/recipe-row rb/all-recipes))))))

(define (rb/recipe-row pair)
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
  (seq
    (load-library "rb")
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
  (let ((costs (rb/get "costs" r)))
    (cons
      (rb/tr (list (rb/th "Name") (rb/th "Per batch") (rb/th "Order") (rb/th "Price") (rb/th "Line cost") (rb/th "Category")))
      (rb/map rb/ingredient-row costs))))

(define (rb/ingredient-row c)
  (rb/tr (list
    (rb/td (rb/get "name" c))
    (rb/td (rb/qty-str (rb/get "qty_per_batch" c)))
    (rb/td (rb/qty-str (rb/get "order_qty" c)))
    (rb/money-cell (rb/get "order_price" c))
    (rb/money-cell (rb/line-cost c))
    (rb/td (rb/get "category" c)))))

(define (rb/step-list r)
  (rb/ul (rb/map rb/step-item (rb/get "steps" r))))

(define (rb/step-item s)
  (let ((name (rb/get "name" s))
        (dur (rb/get "duration" s))
        (note (rb/get "note" s)))
    (rb/li
      (string-append
        (string-append name " (" (rb/qty-str dur) ")")
        (if (null? note) "" (string-append " - " (as-str note)))))))

(define (rb/package-rows r)
  (let ((pkgs (rb/get "packages" r)))
    (cons
      (rb/tr (list (rb/th "Package") (rb/th "Units/batch") (rb/th "L/unit") (rb/th "Price") (rb/th "Margin/unit")))
      (rb/map (lambda (pk) (rb/package-row r pk)) pkgs))))

(define (rb/package-row r pk)
  (let ((biz (rb/business r pk rb/fixed-costs)))
    (rb/tr (list
      (rb/td (rb/get "product_name" pk))
      (rb/td (as-str (rb/get "units_per_batch" pk)))
      (rb/td (rb/qty-str (rb/get "liters_per_unit" pk)))
      (rb/money-cell (rb/get "selling_price" pk))
      (rb/money-cell (rb/contribution-margin-per-unit biz))))))

(define (rb/report-table r)
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
  (seq
    (load-library "rb")
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
  (let ((costs (rb/get "costs" r)))
    (rb/table
      (cons
        (rb/tr (list (rb/th "Ingredient") (rb/th "Order qty") (rb/th "Price") (rb/th "Source")))
        (rb/map rb/shopping-row costs)))))

(define (rb/shopping-row c)
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
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Capital Plan")
        (rb/h2 "Equipment")
        (rb/equipment-table)
        (rb/h2 "Fixed Costs")
        (rb/fixed-cost-table)))))

(define (rb/equipment-table)
  (rb/table
    (cons
      (rb/tr (list (rb/th "Priority") (rb/th "Item") (rb/th "Price") (rb/th "Impact") (rb/th "Unlocks")))
      (rb/map rb/equipment-row rb/equipment-list))))

(define (rb/equipment-row e)
  (rb/tr (list
    (rb/td (as-str (rb/get "priority" e)))
    (rb/td (rb/get "name" e))
    (rb/money-cell (rb/get "price" e))
    (rb/td (let ((i (rb/get "impact" e))) (if (null? i) "" (as-str i))))
    (rb/td (let ((u (rb/get "unlocks" e))) (if (null? u) "" (as-str u)))))))

(define (rb/fixed-cost-table)
  (rb/table
    (cons
      (rb/tr (list (rb/th "Name") (rb/th "Monthly cost")))
      (rb/map rb/fixed-cost-row rb/fixed-costs))))

(define (rb/fixed-cost-row f)
  (rb/tr (list
    (rb/td (rb/get "name" f))
    (rb/money-cell (rb/get "monthly_cost" f)))))

;; ============================================================
;; LABEL PAGE — /label?name=...
;; ============================================================

(define (rb/page-label)
  (seq
    (load-library "rb")
    (let ((name (request-param "name")))
      (if (null? name)
          (html/page (list (rb/h1 "Label not found")))
        (let ((r (rb/find-recipe (as-str name))))
          (if (null? r)
              (html/page (list (rb/h1 "Recipe not found")))
            (render-html (rb/label-html r))))))))

(define (rb/label-html r)
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
;; BATCHES PAGE — /batches
;; ============================================================

(define (rb/page-batches)
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Batch Log")
        (rb/batch-table)))))

(define (rb/batch-table)
  (rb/table
    (cons
      (rb/tr (list (rb/th "Date") (rb/th "Recipe") (rb/th "Yield") (rb/th "Packaged") (rb/th "Rating")))
      (rb/map rb/batch-row rb/batches))))

(define (rb/batch-row b)
  (rb/tr (list
    (rb/td (rb/get "date" b))
    (rb/td (rb/get "recipe" b))
    (rb/td (as-str (rb/get "yield_liters" b)))
    (rb/td (as-str (rb/count (rb/get "packaged" b))))
    (rb/td (as-str (rb/get "rating" b))))))

;; ============================================================
;; PURCHASES PAGE — /purchases
;; ============================================================

(define (rb/page-purchases)
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Purchase Log")
        (rb/purchase-table)))))

(define (rb/purchase-table)
  (rb/table
    (cons
      (rb/tr (list (rb/th "Date") (rb/th "Item") (rb/th "Qty") (rb/th "Price") (rb/th "Source")))
      (rb/map rb/purchase-row rb/purchases))))

(define (rb/purchase-row p)
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
  (cons (cons "type" "order")
  (cons (cons "customer" customer)
  (cons (cons "date" date)
  (cons (cons "line_items" line-items) opts)))))

(define (line-item recipe-name qty price)
  (list
    (cons "recipe" recipe-name)
    (cons "qty" qty)
    (cons "price" price)))

;; seed orders (empty — no historical orders in OCaml rb)
(define rb/orders (list))

;; order persistence
(define (rb/init-order-storage!)
  (if (null? (type-fields (quote order)))
      (deftype (quote order) (quote customer) (quote date) (quote line_items) (quote total) (quote created_day))
    ()))

(define (rb/save-order! ord)
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
  (rb/sum (lambda (li) (* (rb/get "qty" li) (rb/get "price" li)))
          (rb/get "line_items" ord)))

(define (rb/order-rows)
  (rows (quote order)))

;; ============================================================
;; PLANNED VS ACTUAL
;; ============================================================

;; Compare planned profit (from calc) vs actual revenue (from orders).
;; Returns ((planned_revenue . X) (actual_revenue . Y) (variance . Z))
(define (rb/planned-vs-actual)
  (let ((planned (rb/sum (lambda (p) (rb/get "revenue_per_month" (cdr p))) (rb/calc-all))))
    (let ((actual (rb/actual-revenue)))
      (list
        (cons "planned_revenue" planned)
        (cons "actual_revenue" actual)
        (cons "variance" (- actual planned))))))

(define (rb/actual-revenue)
  (rb/sum (lambda (row) (parse-float (dict-get (cdr row) "total"))) (rb/order-rows)))

;; ============================================================
;; ORDERS PAGE — /orders
;; ============================================================

(define (rb/page-orders)
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Orders")
        (rb/order-table)
        (rb/h2 "Planned vs Actual")
        (rb/pva-table)))))

(define (rb/order-table)
  (let ((rws (rb/order-rows)))
    (if (null? rws)
        (rb/p "No orders yet. Sell some soda!")
      (rb/table
        (cons
          (rb/tr (list (rb/th "Date") (rb/th "Customer") (rb/th "Total")))
          (rb/map rb/order-data-row rws))))))

(define (rb/order-data-row row)
  (let ((f (cdr row)))
    (rb/tr (list
      (rb/td (dict-get f "date"))
      (rb/td (dict-get f "customer"))
      (rb/money-cell (parse-float (dict-get f "total")))))))

(define (rb/pva-table)
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
  (seq
    (load-library "rb")
    (html/page
      (list
        (rb/h1 "Business Report")
        (rb/h2 "Monthly Profit by Recipe")
        (rb/report-summary-table)
        (rb/h2 "Plan")
        (rb/plan-table)))))

(define (rb/report-summary-table)
  (rb/table
    (cons
      (rb/tr (list (rb/th "Recipe") (rb/th "Revenue/mo") (rb/th "Cost/mo") (rb/th "Profit/mo") (rb/th "Margin/unit")))
      (rb/map rb/report-summary-row (rb/calc-all)))))

(define (rb/report-summary-row pair)
  (let ((name (car pair)) (report (cdr pair)))
    (rb/tr (list
      (rb/td (rb/link (string-append "/recipe?name=" (html-escape name)) name))
      (rb/money-cell (rb/get "revenue_per_month" report))
      (rb/money-cell (rb/get "total_cost_per_month" report))
      (rb/money-cell (rb/get "profit_per_month" report))
      (rb/money-cell (rb/get "contribution_margin_per_unit" report))))))

(define (rb/plan-table)
  (rb/table
    (list
      (rb/tr (list (rb/th "Metric") (rb/th "Value")))
      (rb/tr (list (rb/td "Batches/month") (rb/td (as-str (rb/get "batches_per_month" rb/plan-config)))))
      (rb/tr (list (rb/td "Starter recipe") (rb/td (rb/get "starter_recipe" rb/plan-config)))))))

;; ============================================================
;; ROUTES
;; ============================================================

(route "/recipes" "rb/page-recipes")
(route "/recipe" "rb/page-recipe-detail")
(route "/shopping" "rb/page-shopping")
(route "/capital" "rb/page-capital")
(route "/label" "rb/page-label")
(route "/batches" "rb/page-batches")
(route "/purchases" "rb/page-purchases")
(route "/orders" "rb/page-orders")
(route "/report" "rb/page-report")
