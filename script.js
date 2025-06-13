// Game state
let currentWord = [];
let currentAttempt = 0;
let gameOver = false;
let words = [];
let candidates = [];
let stats = { played: 0, won: 0, streak: 0, maxStreak: 0, distribution: [0, 0, 0, 0, 0, 0], gameHistory: [] };

// Answer words (lewd words only)
const ANSWER_WORDS = [  "banje", "bants", "beads", "bevvy", "bears", "butch", "campy", "cinch", "clock", "divas", "dolls", "drama", "enbys", "equal", "extra", "flags", "flame", "femmy", "femme", "frock", "gayly", "glitz", "gurls", "heels", "heart", "lippy", "lezzy", "lover", "loves", "mamas", "muggy", "naffs", "otter", "posey", "pride", "proud", "queen", "reads", "shade", "sassy", "slays", "slebs", "snaps", "snogs", "spill", "stole", "studs", "stunt", "sling", "swish", "tease", "tarty", "tucks", "trans", "twerk", "twink", "unity", "werks", "winks", "yasss", "zhuzh", "thicc", "trade", "queen", "motto", "hunty", "queer", "honey", "lanam", "icons", "sissy", "blush", "gloss", "matte", "grwms", "nails", "clack",
];

// All guessable words (includes answer words + common English words)
const GUESSABLE_WORDS = [
  ...ANSWER_WORDS,
  "about", "banje", "sissy", "blush", "gloss", "matte", "grwms", "nails", "clack", "bants", "beads", "bevvy", "bears", "butch", "campy", "cinch", "clock", "divas", "dolls", "drama", "enbys", "equal", "extra", "flags", "flame", "femmy", "femme", "frock", "gayly", "glitz", "gurls", "heels", "heart", "lippy", "lezzy", "lover", "loves", "mamas", "muggy", "naffs", "otter", "posey", "pride", "proud", "queen", "reads", "shade", "sassy", "slays", "slebs", "snaps", "snogs", "spill", "stole", "studs", "stunt", "sling", "swish", "tease", "tarty", "tucks", "trans", "twerk", "twink", "unity", "werks", "winks", "yasss", "zhuzh", "thicc", "trade", "queen", "motto", "hunty", "queer", "honey", "lanam", "icons", "boner", "felch", "pussy", "taint", "semen", "dildo", "farts", "chode", "minge", "gonad", "twats", "spunk", "queef", "prick", "titty", "craps", "balls", "cussy", "sperm", "bulge", "pants", "penis", "arses", "cunts", "knobs", "bussy", "fucks", "jizzy", "wanks", "turds", "shits", "asses", "cocks", "butts", "loads", "booty", "quims", "dicks", "boobs", "lewds", "cunty", "teste", "fanny", "damns", "frick", "shats", "toots", "poops", "shart", "hussy", "dongs", "shaft", "groin", "moobs", "grope", "loins", "horny", "naked", "erect", "pound", "nudes", "hiney", "breed", "cooch", "porno", "moans", "cummy", "labia", "pubes", "boned", "vulva", "clits", "busty", "feces", "teats", "fecal", "screw", "boink", "gooch", "moist", "spank", "whips", "ropes", "lubed", "kinky", "booby", "fucky", "cream", "blows", "snogs", "willy", "milfs", "dilfs", "gilfs", "pissy", "urine", "poopy", "fists", "veiny", "throb", "swing", "thick", "rimmy", "smash", "twerk", "enema", "filth", "choke", "mommy", "daddy", "munch", "handy", "pubic", "thigh", "flaps", "wenis", "hooch", "wench", "skeet", "booba", "potty", "nards", "gaped", "perky", "kegel", "bimbo", "cunny", "shags", "muffs", "himbo", "vixen", "grool", "twink", "cuffs", "cheek", "drool", "pervy", "lover", "doink", "butch", "bitch", "whore", "sluts", "thicc", "girth", "chuff", "fluff", "sound", "crank", "fugly", "tight", "loose", "slurp", "lusty", "lusts", "soapy", "cucks", "skank", "stank", "freak", "gushy", "strip", "tease", "queer", "trick", "sissy", "furry", "spicy", "plops", "vadge", "porks", "thong", "rough", "bound", "chain", "vinyl", "holes", "waxed", "bites", "licks", "plugs", "hairy", "bushy", "bears", "slaps", "latex", "caged", "cages", "panty", "flash", "bulls", "curvy", "spits", "milks", "milky", "doggy", "brats", "nasty", "dirty", "frots", "funky", "messy", "mucky", "above", "agent", "agree", "ahead", "alarm", "album", "alert", "alien", "align", "alike", "alive",
  "allow", "alone", "along", "alter", "among", "anger", "angle", "angry", "apart", "apple",
  "apply", "arena", "argue", "arise", "array", "aside", "asset", "avoid", "awake", "award",
  "aware", "badly", "baker", "bases", "basic", "beach", "began", "begin", "being", "below",
  "bench", "billy", "birth", "black", "blame", "blank", "blind", "block", "blood", "board",
  "boost", "booth", "bound", "brain", "brand", "brass", "brave", "bread", "break", "breed",
  "brief", "bring", "broad", "broke", "brown", "build", "built", "buyer", "cable", "calif",
  "carry", "catch", "cause", "chain", "chair", "chaos", "charm", "chart", "chase", "cheap",
  "check", "chest", "chief", "child", "china", "chose", "civil", "claim", "class", "clean",
  "clear", "click", "climb", "clock", "close", "coach", "coast", "could", "count", "court",
  "cover", "craft", "crash", "crazy", "cream", "crime", "cross", "crowd", "crown", "crude",
  "curve", "cycle", "daily", "dance", "dated", "dealt", "death", "debut", "delay", "depth",
  "doing", "doubt", "dozen", "draft", "drama", "drank", "dream", "dress", "drill", "drink",
  "drive", "drove", "dying", "eager", "early", "earth", "eight", "elite", "empty", "enemy",
  "enjoy", "enter", "entry", "equal", "error", "event", "every", "exact", "exist", "extra",
  "faith", "false", "fault", "fiber", "field", "fifth", "fifty", "fight", "final", "first",
  "fixed", "flash", "fleet", "floor", "fluid", "focus", "force", "forth", "forty", "forum",
  "found", "frame", "frank", "fraud", "fresh", "front", "fruit", "fully", "funny", "giant",
  "given", "glass", "globe", "going", "grace", "grade", "grand", "grant", "grass", "grave",
  "great", "green", "gross", "group", "grown", "guard", "guess", "guest", "guide", "happy",
  "harry", "heart", "heavy", "hence", "henry", "horse", "hotel", "house", "human", "ideal",
  "image", "index", "inner", "input", "issue", "japan", "jimmy", "joint", "jones", "judge",
  "known", "label", "large", "laser", "later", "laugh", "layer", "learn", "lease", "least",
  "leave", "legal", "level", "lewis", "light", "limit", "links", "lives", "local", "loose",
  "lower", "lucky", "lunch", "lying", "magic", "major", "maker", "march", "maria", "match",
  "maybe", "mayor", "meant", "media", "metal", "might", "minor", "minus", "mixed", "model",
  "money", "month", "moral", "motor", "mount", "mouse", "mouth", "moved", "movie", "music",
  "needs", "never", "newly", "night", "noise", "north", "noted", "novel", "nurse", "occur",
  "ocean", "offer", "often", "order", "other", "ought", "paint", "panel", "paper", "paris",
  "parts", "party", "peace", "peter", "phase", "phone", "photo", "piano", "piece", "pilot",
  "pitch", "place", "plain", "plane", "plant", "plate", "plaza", "point", "pound", "power",
  "press", "price", "pride", "prime", "print", "prior", "prize", "proof", "proud", "prove",
  "queen", "quick", "quiet", "quite", "radio", "raise", "range", "rapid", "ratio", "reach",
  "ready", "realm", "rebel", "refer", "relax", "repay", "reply", "right", "rigid", "rival",
  "river", "robin", "roger", "roman", "rough", "round", "route", "royal", "rural", "scale",
  "scene", "scope", "score", "sense", "serve", "seven", "shall", "shape", "share", "sharp",
  "sheet", "shelf", "shell", "shine", "shirt", "shock", "shoot", "short", "shown", "sides",
  "sight", "simon", "since", "sixth", "sixty", "sized", "skill", "sleep", "slide", "small",
  "smart", "smile", "smith", "smoke", "solid", "solve", "sorry", "sound", "south", "space",
  "spare", "speak", "speed", "spend", "spent", "split", "spoke", "sport", "staff", "stage",
  "stake", "stand", "start", "state", "steam", "steel", "steep", "steer", "steve", "stick",
  "still", "stock", "stone", "stood", "store", "storm", "story", "strip", "stuck", "study",
  "stuff", "style", "sugar", "suite", "super", "sweet", "table", "taken", "taste", "taxes",
  "teach", "teams", "teeth", "terry", "texas", "thank", "theft", "their", "theme", "there",
  "these", "thick", "thing", "think", "third", "those", "three", "threw", "throw", "thumb",
  "tight", "times", "tired", "title", "today", "topic", "total", "touch", "tough", "tower",
  "track", "trade", "train", "treat", "trend", "trial", "tribe", "trick", "tried", "tries",
  "truck", "truly", "trust", "truth", "twice", "uncle", "under", "undue", "union", "unity",
  "until", "upper", "upset", "urban", "usage", "usual", "valid", "value", "video", "virus",
  "visit", "vital", "vocal", "voice", "waste", "watch", "water", "wheel", "where", "which",
  "while", "white", "whole", "whose", "woman", "women", "world", "worry", "worse", "worst",
  "worth", "would", "write", "wrong", "wrote", "young", "youth",
  // Additional comprehensive dictionary
  "abbey", "abode", "aback", "abate", "abhor", "abled", "abode", "abort", "about", "above",
  "abuse", "abyss", "acorn", "acted", "actor", "acute", "adage", "adapt", "added", "adder",
  "adopt", "adore", "adult", "afar", "after", "again", "agent", "agile", "aging", "aglow",
  "agree", "ahead", "aided", "aimed", "aired", "aisle", "alarm", "album", "alert", "alias",
  "alibi", "alien", "align", "alike", "alive", "alley", "alloy", "allow", "alone", "along",
  "aloof", "alpha", "altar", "alter", "amber", "amble", "amend", "amino", "among", "ample",
  "amuse", "angel", "anger", "angle", "angry", "ankle", "annex", "antic", "apart", "aphid",
  "apple", "apply", "apron", "aptly", "arbor", "arch", "arena", "argue", "arise", "armed",
  "armor", "aroma", "arose", "array", "arrow", "arson", "artsy", "ascot", "ashen", "aside",
  "askew", "aspen", "asset", "atoll", "atone", "attic", "audio", "audit", "augur", "aunty",
  "avail", "avert", "avoid", "await", "awake", "award", "aware", "awash", "awful", "awoke",
  "badge", "badly", "bagel", "baker", "balmy", "banjo", "barge", "baron", "basic", "basil",
  "batch", "bathe", "baton", "beach", "beast", "began", "begin", "being", "belly", "below",
  "bench", "berry", "bikes", "billy", "bingo", "birth", "black", "blade", "blame", "bland",
  "blank", "blast", "blaze", "bleak", "bleat", "blend", "bless", "blimp", "blind", "blink",
  "bliss", "blitz", "bloat", "block", "blood", "bloom", "blown", "blues", "bluff", "blunt",
  "blurb", "blurt", "blush", "board", "boast", "bobby", "bogey", "boils", "bombs", "bonds",
  "boned", "bongo", "bonus", "booby", "boost", "booth", "boots", "borax", "borne", "bosom",
  "bossy", "botch", "bough", "bound", "boxed", "boxer", "brace", "braid", "brain", "brake",
  "brand", "brash", "brass", "brave", "bravo", "brawl", "bread", "break", "breed", "brief",
  "brine", "bring", "brink", "briny", "brisk", "broad", "broil", "broke", "brood", "brook",
  "broom", "brown", "brush", "build", "built", "bulge", "bulky", "bunch", "bunny", "burly",
  "burnt", "burst", "buses", "butch", "buyer", "bylaw", "cabal", "cabin", "cable", "cache",
  "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", "canny", "canoe", "canon",
  "caper", "caput", "carat", "cargo", "carol", "carry", "carve", "catch", "cater", "cause",
  "cavil", "cease", "cedar", "chair", "champ", "chant", "chaos", "chard", "charm", "chart",
  "chase", "chasm", "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick",
  "chief", "child", "chill", "chimp", "china", "chirp", "chive", "chock", "choir", "choke",
  "chord", "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider", "cigar",
  "cinch", "circa", "cited", "civic", "civil", "clack", "claim", "clamp", "clang", "clank",
  "clash", "clasp", "class", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff",
  "climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout",
  "clown", "clubs", "cluck", "clued", "clump", "clung", "coach", "coast", "cocoa", "coded",
  "coder", "codex", "colon", "color", "comet", "comic", "conch", "condo", "coral", "corgi",
  "corny", "couch", "cough", "could", "count", "court", "coven", "cover", "craft", "cramp",
  "crane", "crank", "crash", "crass", "crate", "crave", "crawl", "craze", "crazy", "creak",
  "cream", "creed", "creek", "creep", "creme", "crepe", "cress", "crest", "crick", "cried",
  "crimp", "crisp", "croak", "crock", "croft", "crone", "crony", "crook", "cross", "crowd",
  "crown", "crude", "cruel", "crumb", "crunk", "crush", "crust", "cubic", "cumin", "cupid",
  "curly", "curry", "curse", "curve", "curvy", "cyber", "cycle", "cynic", "daddy", "daily",
  "dairy", "daisy", "dally", "dance", "dandy", "datum", "daunt", "dealt", "death", "debit",
  "debug", "debut", "decal", "decay", "decor", "decoy", "decree", "defer", "deign", "deity",
  "delay", "delta", "delve", "demon", "demur", "denim", "dense", "depot", "depth", "derby",
  "deter", "detox", "deuce", "diary", "dicey", "digit", "dimly", "diner", "dingo", "dingy",
  "diode", "dirge", "dirty", "disco", "ditch", "ditto", "ditty", "diver", "dizzy", "dodge",
  "doing", "dolly", "donor", "donut", "dopey", "doubt", "dough", "douse", "dowdy", "dower",
  "downy", "dowry", "dozen", "draft", "drain", "drake", "drama", "drank", "drape", "drawl",
  "drawn", "dread", "dream", "dregs", "dress", "dried", "drier", "drift", "drill", "drink",
  "drive", "droit", "droll", "drone", "drool", "droop", "drove", "drown", "druid", "drunk",
  "dryer", "dryly", "duchy", "ducky", "dummy", "dumpy", "dunce", "dusky", "dusty", "dutch",
  "duvet", "dwarf", "dwell", "dying", "eager", "eagle", "early", "earth", "easel", "eaten",
  "eater", "ebony", "ecard", "edict", "edify", "eerie", "egret", "eight", "eject", "eking",
  "elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "email",
  "embed", "ember", "emcee", "empty", "enact", "endow", "enema", "enemy", "enjoy", "ennui",
  "ensue", "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect",
  "erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade",
  "event", "every", "evict", "evoke", "exact", "exalt", "excel", "exert", "exile", "exist",
  "expat", "expel", "extra", "exult", "fable", "facet", "faded", "fairy", "faith", "false",
  "fancy", "fanny", "farce", "fatal", "fatty", "fault", "fauna", "favor", "feast", "fecal",
  "feign", "fella", "felon", "femme", "fence", "feral", "ferry", "fetal", "fetch", "fetid",
  "fetus", "fever", "fewer", "fiber", "ficus", "field", "fiend", "fiery", "fifth", "fifty",
  "fight", "filch", "filer", "filly", "filmy", "filth", "final", "finch", "finer", "first",
  "fishy", "fixer", "fizzy", "fjord", "flack", "flail", "flair", "flake", "flaky", "flame",
  "flank", "flare", "flash", "flask", "fleck", "fleet", "flesh", "flick", "flier", "fling",
  "flint", "flirt", "float", "flock", "flood", "floor", "flour", "flout", "flown", "fluff",
  "fluid", "fluke", "flume", "flung", "flunk", "flush", "flute", "flyer", "foamy", "focal",
  "focus", "foggy", "foist", "folio", "folly", "foray", "force", "forge", "forgo", "forte",
  "forth", "forty", "forum", "found", "foyer", "frail", "frame", "frank", "fraud", "freak",
  "freed", "freer", "fresh", "friar", "fried", "frill", "frisk", "fritz", "frock", "frolic",
  "front", "frost", "frown", "froze", "fruit", "fudge", "fugue", "fully", "fungi", "funky",
  "funny", "furry", "fussy", "fuzzy", "gaffe", "gaily", "gamer", "gamma", "gamut", "gassy",
  "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawky", "geeky", "genie", "genre", "ghost",
  "ghoul", "giant", "giddy", "gipsy", "girly", "girth", "given", "giver", "gizmo", "glade",
  "gland", "glare", "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat", "globe",
  "gloom", "glory", "gloss", "glove", "glyph", "gnome", "goaty", "godly", "going", "golem",
  "golly", "gonad", "goner", "goody", "gooey", "goofy", "goose", "gorge", "gotcha", "gouge",
  "gourd", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph",
  "grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "greek", "green",
  "greet", "grief", "grill", "grime", "grimy", "grind", "gripe", "groan", "groin", "groom",
  "grope", "gross", "group", "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt",
  "guard", "guava", "guess", "guest", "guide", "guild", "guilt", "guise", "gulch", "gully",
  "gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "haiku", "haint", "hairy",
  "halal", "halve", "handy", "happy", "hardy", "harem", "harpy", "harsh", "haste", "hasty",
  "hatch", "hater", "haunt", "haute", "haven", "havoc", "hawse", "hayed", "hazel", "heady",
  "heard", "heart", "heath", "heave", "heavy", "hedge", "hefty", "helix", "hello", "hence",
  "henry", "herby", "hertz", "hiccup", "hider", "highs", "hiker", "hilts", "hinds", "hinge",
  "hippo", "hippy", "hitch", "hoard", "hobby", "hoist", "holly", "homer", "honey", "honor",
  "hooey", "hooks", "hooky", "horde", "horny", "horse", "hotel", "hound", "house", "hovel",
  "hover", "howdy", "human", "humid", "humor", "humph", "humus", "hunch", "hunks", "hurry",
  "husky", "hussy", "hutch", "hyena", "hymen", "hyper", "ichor", "icing", "ideal", "idiom",
  "idiot", "idler", "igloo", "ileum", "image", "imbue", "impel", "inane", "inbox", "incur",
  "index", "inept", "inert", "infer", "infix", "ingot", "inked", "inlay", "inlet", "inner",
  "input", "inset", "inter", "intro", "ionic", "irate", "irony", "islet", "issue", "itchy",
  "ivory", "jazzy", "jeans", "jelly", "jerky", "jetty", "jewel", "jiffy", "joint", "joist",
  "joker", "jolly", "joust", "judge", "juice", "juicy", "jumbo", "jumpy", "junco", "junky",
  "kappa", "karma", "kayak", "kebab", "khaki", "kiddo", "kinky", "kiosk", "kitty", "klutz",
  "knack", "knave", "knead", "kneel", "knelt", "knife", "knock", "knoll", "known", "koala",
  "krill", "label", "labor", "laden", "ladle", "lager", "lance", "lanky", "lapel", "lapse",
  "large", "larva", "laser", "lasso", "latch", "later", "lathe", "latte", "laugh", "layer",
  "leach", "leafy", "leaky", "leant", "leapt", "learn", "lease", "leash", "least", "leave",
  "ledge", "leech", "leery", "lefty", "legal", "leggy", "lemon", "lemur", "leper", "level",
  "lever", "libel", "liege", "light", "liken", "lilac", "limbo", "limit", "linen", "liner",
  "lingo", "links", "lints", "lions", "lipid", "lists", "liter", "lithe", "litre", "lived",
  "liver", "lives", "llama", "loamy", "loans", "loath", "lobby", "local", "locus", "lodge",
  "lofty", "logic", "login", "loopy", "loose", "lorry", "loser", "lotto", "lotus", "louse",
  "lousy", "lover", "lower", "lowly", "loyal", "lucid", "lucky", "lumen", "lumps", "lunar",
  "lunch", "lunge", "lungs", "lurch", "lurid", "lusty", "lying", "lymph", "lynch", "lyric",
  "macaw", "macho", "macro", "madam", "madly", "mafia", "magic", "magma", "maids", "major",
  "maker", "mamba", "mambo", "mango", "mangy", "mania", "manic", "manly", "manor", "maple",
  "march", "marco", "maria", "marry", "marsh", "mason", "match", "matey", "maths", "matte",
  "mauve", "maxim", "maybe", "mayor", "mealy", "meant", "meaty", "mecca", "medal", "media",
  "medic", "melee", "melon", "mercy", "merge", "merit", "merry", "metal", "meter", "metro",
  "micro", "midge", "midst", "might", "milky", "mimic", "mince", "miner", "minim", "minor",
  "minus", "mirth", "missy", "miter", "mitre", "mixed", "mixer", "mocha", "modal", "model",
  "modem", "mogul", "moist", "molar", "moldy", "money", "mongo", "monit", "mooch", "moody",
  "moose", "moral", "moron", "morse", "mossy", "motel", "motif", "motor", "motto", "mould",
  "mound", "mount", "mouse", "mousy", "mouth", "moved", "mover", "movie", "moxie", "mucus",
  "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy", "music", "musky", "musty",
  "myrrh", "nadir", "naive", "nanny", "nasal", "nasty", "natal", "natch", "navel", "needy",
  "neigh", "nerdy", "never", "newer", "newly", "nicer", "niche", "niece", "night", "ninja",
  "ninth", "noble", "nobly", "nodes", "noise", "noisy", "nomad", "noose", "north", "nosey",
  "notch", "noted", "noun", "novel", "nurse", "nutty", "nylon", "nymph", "oaken", "obese",
  "occur", "ocean", "octal", "octet", "odder", "offal", "offer", "often", "olden", "older",
  "olive", "ombre", "omega", "onion", "onset", "opera", "opine", "opium", "optic", "orbit",
  "order", "organ", "other", "otter", "ought", "ounce", "outdo", "outer", "outgo", "ovary",
  "ovine", "ovoid", "owing", "owner", "oxide", "ozone", "paddy", "pagan", "pager", "palsy",
  "panel", "panic", "pansy", "pants", "panty", "paper", "parch", "parry", "parse", "party",
  "pasta", "paste", "pasty", "patch", "pater", "patio", "patsy", "patty", "pause", "payee",
  "payer", "peace", "peach", "pearl", "pecan", "pedal", "penal", "penny", "perch", "peril",
  "perky", "perms", "pest", "petal", "petty", "phase", "phone", "phony", "photo", "piano",
  "picky", "piece", "piety", "piggy", "pilot", "pinch", "piney", "pinky", "pinto", "piper",
  "pique", "pitch", "pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain",
  "plait", "plane", "plank", "plant", "plate", "plaza", "plead", "pleat", "plied", "pliny",
  "plonk", "plop", "plot", "pluck", "plumb", "plume", "plump", "plunk", "plush", "poach",
  "pock", "podgy", "point", "poker", "polar", "polka", "polyp", "pooch", "poppy", "porch",
  "porky", "posse", "pouch", "pound", "pouty", "power", "prank", "prawn", "press", "price",
  "prick", "pride", "pried", "prime", "primo", "print", "prior", "privy", "prize", "probe",
  "prone", "proof", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm",
  "pubic", "pudgy", "puffy", "pulpy", "pulse", "punch", "punny", "pupil", "puppy", "purge",
  "purse", "pushy", "putty", "pygmy", "quack", "quail", "quake", "qualm", "quart", "quasi",
  "queen", "queer", "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt",
  "quirk", "quite", "quota", "quote", "rabid", "racer", "radar", "radii", "radio", "rainy",
  "raise", "rally", "ralph", "ramen", "ranch", "randy", "range", "rapid", "rarer", "raspy",
  "ratio", "ratty", "raven", "rayon", "razor", "reach", "react", "ready", "realm", "rearm",
  "rebar", "rebel", "rebus", "rebut", "recap", "recur", "recut", "redid", "reedy", "reefs",
  "reek", "refer", "refit", "regal", "rehab", "reign", "relax", "relay", "relic", "remit",
  "remix", "repay", "repel", "reply", "rerun", "reset", "resin", "retch", "retro", "retry",
  "reuse", "revel", "revue", "rhino", "rhyme", "rider", "ridge", "rifle", "right", "rigid",
  "rigor", "rinse", "ripen", "riper", "risen", "riser", "risky", "rival", "river", "rivet",
  "roach", "roast", "robed", "robin", "robot", "rocky", "rodeo", "roger", "rogue", "roman",
  "romp", "rooky", "roomy", "roost", "rope", "roses", "rosy", "rote", "rotor", "rouge",
  "rough", "round", "rouse", "route", "rover", "rowdy", "rower", "royal", "rub", "rugby",
  "ruler", "rumba", "rumor", "rupee", "rural", "rushy", "rusty", "sadly", "safer", "saint",
  "salad", "sally", "salon", "salsa", "salty", "salve", "salvo", "sandy", "saner", "sappy",
  "sarge", "sass", "sassy", "satan", "satin", "satyr", "sauce", "saucy", "sauna", "saute",
  "savey", "savor", "savvy", "scald", "scale", "scalp", "scamp", "scant", "scare", "scarf",
  "scary", "scene", "scent", "scion", "scoff", "scone", "scoop", "scope", "score", "scorn",
  "scour", "scout", "scowl", "scram", "scrap", "scree", "screw", "scrub", "scuba", "scuff",
  "seamy", "sear", "seats", "secco", "sedan", "seedy", "segue", "seize", "semen", "sense",
  "sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewer", "shack", "shade",
  "shady", "shaft", "shake", "shaky", "shale", "shall", "shame", "shank", "shape", "shard",
  "share", "shark", "sharp", "shave", "shawl", "shear", "sheen", "sheep", "sheer", "sheet",
  "shelf", "shell", "shine", "shiny", "shire", "shirk", "shirt", "shock", "shone", "shook",
  "shoot", "shore", "shorn", "short", "shout", "shove", "shown", "showy", "shred", "shrew",
  "shrub", "shrug", "shuck", "shunt", "shush", "sicko", "sidle", "siege", "sight", "sigma",
  "silky", "silly", "since", "sinew", "singe", "siren", "sissy", "sixth", "sixty", "skate",
  "skeet", "skein", "skimp", "skink", "skint", "skirt", "skulk", "skull", "skunk", "slack",
  "slain", "slang", "slant", "slash", "slate", "slave", "sleek", "sleep", "sleet", "slept",
  "slice", "slick", "slide", "slime", "slimy", "sling", "slink", "slosh", "sloth", "slump",
  "slung", "slunk", "slurp", "slush", "slyly", "smack", "small", "smart", "smash", "smear",
  "smell", "smelt", "smile", "smirk", "smite", "smith", "smock", "smoke", "smoky", "smolt",
  "snack", "snafu", "snail", "snake", "snaky", "snare", "snarl", "sneak", "sneer", "snide",
  "sniff", "snipe", "snore", "snort", "snout", "snowy", "snuck", "snuff", "soapy", "sober",
  "soggy", "solar", "solve", "sonar", "sonic", "soothe", "sooty", "sorry", "sound", "south",
  "space", "spade", "spank", "spare", "spark", "spasm", "spawn", "speak", "spear", "speck",
  "speed", "spell", "spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike",
  "spiky", "spill", "spilt", "spine", "spiny", "spire", "spite", "split", "spoil", "spoke",
  "spoof", "spook", "spool", "spoon", "spore", "sport", "spout", "spray", "spree", "sprig",
  "spunk", "spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage", "staid",
  "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank", "staph",
  "stare", "stark", "start", "stash", "state", "stave", "stays", "stead", "steak", "steal",
  "steam", "steed", "steel", "steep", "steer", "stein", "stern", "stick", "stiff", "still",
  "stilt", "sting", "stink", "stint", "stock", "stoic", "stoke", "stole", "stomp", "stone",
  "stood", "stool", "stoop", "store", "stork", "storm", "story", "stout", "stove", "strap",
  "straw", "stray", "strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk",
  "stunt", "style", "suave", "sugar", "suing", "suite", "sulky", "sully", "sumac", "sunny",
  "super", "surer", "surf", "surge", "surly", "sushi", "swab", "swag", "swamp", "swank",
  "swarm", "swash", "swath", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift",
  "swill", "swine", "swing", "swipe", "swirl", "swish", "swiss", "swoon", "swoop", "sword",
  "swore", "sworn", "swung", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacky",
  "taffy", "taint", "taken", "taker", "tango", "tangy", "tank", "tape", "tardy", "tarot",
  "taste", "tasty", "tatty", "taunt", "tawny", "teach", "teary", "tease", "teddy", "teens",
  "teeny", "teeth", "tempo", "tenet", "tenor", "tense", "tenth", "tepid", "terms", "terns",
  "terra", "terse", "tests", "thank", "thaw", "theft", "their", "theme", "there", "these",
  "thick", "thief", "thigh", "thing", "think", "third", "thong", "thorn", "those", "three",
  "threw", "throb", "throw", "thrum", "thud", "thug", "thumb", "thump", "thunk", "thus",
  "thyme", "tiara", "tibia", "tidal", "tiger", "tight", "tilde", "timer", "timid", "tipsy",
  "titan", "tithe", "title", "toast", "today", "toddy", "token", "tonal", "tonga", "tonic",
  "tooth", "topaz", "topic", "torch", "torso", "total", "totem", "touch", "tough", "towel",
  "tower", "toxic", "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait",
  "tramp", "trash", "treat", "trend", "tress", "triad", "trial", "tribe", "trick", "tried",
  "trims", "trio", "trite", "trod", "troll", "troop", "trope", "trout", "trove", "truck",
  "truce", "trudge", "truly", "trump", "trunk", "trust", "truth", "twice", "uncle", "under",
  "undid", "undue", "unfed", "unfit", "unify", "union", "unite", "unity", "unlit", "unmet",
  "unset", "until", "unwed", "unzip", "upper", "upset", "urban", "urged", "urine", "usage",
  "usher", "using", "usual", "usurp", "utter", "vague", "valet", "valid", "valor", "value",
  "valve", "vapor", "vault", "vaunt", "vegan", "vein", "venom", "venue", "verge", "verse",
  "verso", "vertu", "vetch", "vexed", "vibes", "vicar", "video", "vied", "views", "vigil",
  "vigor", "vile", "vinyl", "viola", "viper", "viral", "virus", "visit", "visor", "vista",
  "vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "void", "voila", "vomit",
  "voter", "vouch", "vowel", "vying", "wacky", "waddy", "wager", "wages", "wagon", "waist",
  "waits", "waive", "waken", "waker", "walks", "waltz", "wands", "waned", "wards", "wares",
  "warms", "warns", "warps", "warts", "waste", "watch", "water", "waved", "waver", "waves",
  "waxed", "waxes", "weary", "weave", "webby", "wedge", "weedy", "weeks", "weepy", "weigh",
  "weird", "wells", "welsh", "wench", "whack", "whale", "wharf", "wheat", "wheel", "whelm",
  "where", "which", "whiff", "while", "whims", "whine", "whiny", "whips", "whirl", "whirr",
  "whisk", "whist", "white", "whits", "whole", "whomp", "whoop", "whore", "whose", "wicks",
  "widen", "wider", "widow", "width", "wield", "wight", "wiles", "wills", "wilts", "wimpy",
  "wince", "winch", "winds", "windy", "wines", "wings", "winks", "wiped", "wiper", "wipes",
  "wired", "wires", "wiser", "wisps", "wispy", "witch", "wives", "wizen", "wolds", "woman",
  "women", "wonky", "woods", "woody", "wooer", "woofs", "wools", "wooly", "woozy", "words",
  "wordy", "works", "world", "worms", "wormy", "worn", "worry", "worse", "worst", "worth",
  "would", "wound", "woven", "wrack", "wraps", "wrath", "wreak", "wreck", "wrens", "wrest",
  "wrier", "wring", "wrist", "write", "wrong", "wrote", "wrung", "wryly", "yacht", "yacks",
  "yaffs", "yager", "yales", "yams", "yanks", "yards", "yarns", "yawed", "yawls", "yawns",
  "yeahs", "years", "yeast", "yecch", "yells", "yelps", "yeses", "yetis", "yield", "yipes",
  "yodel", "yogas", "yokes", "yolks", "young", "yours", "youth", "yowls", "yucks", "yummy",
  "zaire", "zappy", "zebra", "zebus", "zests", "zesty", "zilch", "zincs", "zings", "zingy",
  "zippy", "zitis", "zonal", "zoned", "zones", "zonks", "zooks", "zooms", "zowie", "zulus"
];

const CANDIDATES = [...ANSWER_WORDS];

// Initialize game
function onInit() {
  loadStats();
  setupEventListeners();
  startNewGame();
}

function loadStats() {
  const saved = localStorage.getItem('lewdle-stats');
  if (saved) {
    stats = { ...stats, ...JSON.parse(saved) };
  }
}

function saveStats() {
  localStorage.setItem('lewdle-stats', JSON.stringify(stats));
}

function setupEventListeners() {
  // Keyboard events
  document.addEventListener('keydown', onKeyDown);

  // Button events
  document.getElementById('howtoplay').addEventListener('click', () => showOverlay('howtoplay'));
  document.getElementById('stats').addEventListener('click', showStats);
  document.getElementById('settings').addEventListener('click', () => showOverlay('settings'));

  // Modal close events
  document.querySelector('.modal-close').addEventListener('click', hideModal);
  document.querySelector('.howtoplay-close').addEventListener('click', hideOverlay);
  document.querySelector('.settings-close').addEventListener('click', hideOverlay);
  document.querySelector('.backdrop').addEventListener('click', hideAll);

  // Play again button
  document.getElementById('playagain').addEventListener('click', onClickPlayAgain);
}

function startNewGame() {
  // Reset game state
  currentWord = [];
  currentAttempt = 0;
  gameOver = false;
  words = [];
  candidates = [...CANDIDATES];

  // Clear UI
  document.querySelector('.words').innerHTML = '';
  document.body.classList.remove('game-over');

  // Create word grid
  for (let i = 0; i < 6; i++) {
    createWordRow();
  }

  // Create keyboard
  createKeyboard();

  // Choose random word
  chooseWord();

  updateCandidatesDisplay();
}

function createWordRow() {
  const wordsContainer = document.querySelector('.words');
  const wordDiv = document.createElement('div');
  wordDiv.className = 'word';

  for (let i = 0; i < 5; i++) {
    const letterDiv = document.createElement('div');
    letterDiv.className = 'letter';
    wordDiv.appendChild(letterDiv);
  }

  wordsContainer.appendChild(wordDiv);
  words.push([]);
}

function createKeyboard() {
  const keyboard = document.querySelector('.keyboard');
  keyboard.innerHTML = '';

  const rows = [
    ['Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P'],
    ['A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L'],
    ['ENTER', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', 'DEL']
  ];

  rows.forEach(row => {
    const rowDiv = document.createElement('div');
    rowDiv.className = 'keyboard-row';

    row.forEach(key => {
      const button = document.createElement('button');
      button.className = 'keyboard-button';
      button.textContent = key;

      if (key === 'ENTER' || key === 'DEL') {
        button.classList.add('larger');
      }

      button.addEventListener('click', () => handleKeyPress(key));
      rowDiv.appendChild(button);
    });

    keyboard.appendChild(rowDiv);
  });
}

function chooseWord() {
  const randomIndex = Math.floor(Math.random() * ANSWER_WORDS.length);
  window.targetWord = ANSWER_WORDS[randomIndex].toUpperCase();
  console.log('Target word:', window.targetWord); // For debugging
}

function onKeyDown(event) {
  if (gameOver) return;

  const key = event.key.toUpperCase();

  if (key === 'ENTER') {
    handleKeyPress('ENTER');
  } else if (key === 'BACKSPACE') {
    handleKeyPress('DEL');
  } else if (key.match(/[A-Z]/) && key.length === 1) {
    handleKeyPress(key);
  }
}

function handleKeyPress(key) {
  if (gameOver) return;

  if (key === 'ENTER') {
    submitWord();
  } else if (key === 'DEL') {
    deleteLetter();
  } else if (key.match(/[A-Z]/) && currentWord.length < 5) {
    addLetter(key);
  }
}

function addLetter(letter) {
  if (currentWord.length < 5) {
    currentWord.push(letter);
    updateDisplay();
  }
}

function deleteLetter() {
  if (currentWord.length > 0) {
    currentWord.pop();
    updateDisplay();
  }
}

function updateDisplay() {
  const wordElements = document.querySelectorAll('.word')[currentAttempt].querySelectorAll('.letter');

  for (let i = 0; i < 5; i++) {
    wordElements[i].textContent = currentWord[i] || '';
  }
}

function submitWord() {
  if (currentWord.length !== 5) {
    showToast('Not enough letters');
    return;
  }

  const wordString = currentWord.join('');

  if (!GUESSABLE_WORDS.includes(wordString.toLowerCase())) {
    showToast('Not in word list');
    return;
  }

  // Store the word
  words[currentAttempt] = [...currentWord];

  // Mark word as attempted
  document.querySelectorAll('.word')[currentAttempt].classList.add('attempted');

  // Check letters (this now handles all game logic including win/lose)
  checkWord();
}

function checkWord() {
  const wordElements = document.querySelectorAll('.word')[currentAttempt].querySelectorAll('.letter');
  const targetArray = window.targetWord.split('');
  const guessArray = [...currentWord];
  const letterCounts = {};
  const results = [];

  // Count letters in target word
  targetArray.forEach(letter => {
    letterCounts[letter] = (letterCounts[letter] || 0) + 1;
  });

  // First pass: determine correct letters and their results
  for (let i = 0; i < 5; i++) {
    if (guessArray[i] === targetArray[i]) {
      results[i] = 'correct';
      letterCounts[guessArray[i]]--;
    } else {
      results[i] = null; // Will determine in second pass
    }
  }

  // Second pass: determine semi-correct and incorrect letters
  for (let i = 0; i < 5; i++) {
    if (results[i] === null) {
      if (letterCounts[guessArray[i]] > 0) {
        results[i] = 'semi';
        letterCounts[guessArray[i]]--;
      } else {
        results[i] = 'incorrect';
      }
    }
  }

  // Animate each letter reveal with delay
  results.forEach((result, index) => {
    setTimeout(() => {
      wordElements[index].classList.add(result);
      updateKeyboard(guessArray[index], result);

      // Add flip animation
      wordElements[index].style.animation = 'letter-flip 0.6s ease-in-out';

      // If this is the last letter, proceed with game logic
      if (index === 4) {
        setTimeout(() => {
          // Filter candidates
          filterCandidates();

          // Check win/lose conditions
          const wordString = currentWord.join('');
          if (wordString === window.targetWord) {
            // Win
            gameOver = true;
            document.body.classList.add('game-over');
            document.getElementById('word-answer').textContent = window.targetWord;

            // Update stats
            stats.played++;
            stats.won++;
            stats.streak++;
            stats.maxStreak = Math.max(stats.maxStreak, stats.streak);
            stats.distribution[currentAttempt]++;
            
            // Add to game history
            stats.gameHistory.unshift({
              word: window.targetWord,
              attempts: currentAttempt + 1,
              won: true,
              date: new Date().toLocaleDateString()
            });
            
            // Keep only last 20 games
            if (stats.gameHistory.length > 20) {
              stats.gameHistory = stats.gameHistory.slice(0, 20);
            }
            
            saveStats();

            showToast('Excellent!');
            
            // Show stats after a brief delay
            setTimeout(() => {
              showStats();
            }, 1500);
          } else if (currentAttempt === 5) {
            // Lose
            gameOver = true;
            document.body.classList.add('game-over');
            document.getElementById('word-answer').textContent = window.targetWord;

            // Update stats
            stats.played++;
            stats.streak = 0;
            
            // Add to game history
            stats.gameHistory.unshift({
              word: window.targetWord,
              attempts: 6,
              won: false,
              date: new Date().toLocaleDateString()
            });
            
            // Keep only last 20 games
            if (stats.gameHistory.length > 20) {
              stats.gameHistory = stats.gameHistory.slice(0, 20);
            }
            
            saveStats();

            showToast(`The word was ${window.targetWord}`);
            
            // Show stats after a brief delay
            setTimeout(() => {
              showStats();
            }, 1500);
          } else {
            currentAttempt++;
            currentWord = [];
          }

          updateCandidatesDisplay();
        }, 100);
      }
    }, index *375); // 375ms delay between each letter
  });
}

function updateKeyboard(letter, status) {
  const buttons = document.querySelectorAll('.keyboard-button');
  buttons.forEach(button => {
    if (button.textContent === letter && !button.classList.contains('correct')) {
      button.classList.remove('semi', 'incorrect');
      button.classList.add(status);
    }
  });
}

function filterCandidates() {
  const guessedWord = words[currentAttempt].join('');
  const targetWord = window.targetWord;

  candidates = candidates.filter(candidate => {
    const candidateUpper = candidate.toUpperCase();

    // Check if this candidate would produce the same pattern
    for (let i = 0; i < 5; i++) {
      const guessLetter = guessedWord[i];
      const targetLetter = targetWord[i];
      const candidateLetter = candidateUpper[i];

      if (guessLetter === targetLetter) {
        // Correct position - candidate must have same letter in same position
        if (candidateLetter !== guessLetter) return false;
      } else if (targetWord.includes(guessLetter)) {
        // Wrong position but letter exists - candidate must have letter but not in this position
        if (!candidateUpper.includes(guessLetter) || candidateLetter === guessLetter) return false;
      } else {
        // Letter doesn't exist - candidate must not have this letter
        if (candidateUpper.includes(guessLetter)) return false;
      }
    }

    return true;
  });
}

function updateCandidatesDisplay() {
  const element = document.getElementById('candidates-left');
  element.textContent = `${candidates.length} candidates left`;
}

function showToast(message) {
  const toast = document.createElement('div');
  toast.className = 'toast';
  toast.textContent = message;
  document.body.appendChild(toast);

  setTimeout(() => {
    document.body.removeChild(toast);
  }, 2000);
}

function showStats() {
  const modal = document.querySelector('.modal');
  const title = document.querySelector('.modal-title');
  const content = document.querySelector('.modal-content');

  title.textContent = 'Statistics';

  const winRate = stats.played > 0 ? Math.round((stats.won / stats.played) * 100) : 0;

  content.innerHTML = `
    <div class="statistics">
      <span>${stats.played}</span>
      <span>Games\nPlayed</span>
      <span>${winRate}</span>
      <span>Win %</span>
      <span>${stats.streak}</span>
      <span>Current\nStreak</span>
      <span>${stats.maxStreak}</span>
      <span>Max\nStreak</span>
    </div>
    <div class="distribution">
      ${stats.distribution.map((count, index) => `
        <span>${index + 1}</span>
        <div style="background: ${count > 0 ? '#538d4e' : '#3a3a3c'}; width: ${Math.max(count * 20, 10)}px;">
          <span>${count}</span>
        </div>
      `).join('')}
    </div>
    ${stats.gameHistory && stats.gameHistory.length > 0 ? `
      <div class="game-history">
        <div class="history-title">Recent Games</div>
        ${stats.gameHistory.slice(0, 10).map(game => `
          <div class="history-item">
            <span class="history-word">${game.word}</span>
            <span class="history-attempts ${game.won ? 'won' : 'lost'}">${game.won ? game.attempts + '/6' : 'X/6'}</span>
            <span class="history-date">${game.date}</span>
          </div>
        `).join('')}
      </div>
    ` : ''}
    <button class="share-button" onclick="shareResults()">
      <span>Share</span>
      <span>📋</span>
    </button>
  `;

  showModal();
}

function shareResults() {
  const attempts = gameOver ? (words.findIndex(word => word.join('') === window.targetWord) + 1) : 'X';
  const squares = words.slice(0, currentAttempt).map(word => {
    return word.map((letter, index) => {
      if (letter === window.targetWord[index]) return '🟩';
      if (window.targetWord.includes(letter)) return '🟨';
      return '⬛';
    }).join('');
  }).join('\n');

  const result = `Lewdle ${attempts}/6\n\n${squares}`;

  if (navigator.share) {
    navigator.share({ text: result });
  } else {
    navigator.clipboard.writeText(result).then(() => {
      showToast('Copied to clipboard!');
    });
  }
}

function showModal() {
  document.querySelector('.modal').classList.add('show');
  document.querySelector('.backdrop').classList.add('show');
}

function hideModal() {
  document.querySelector('.modal').classList.remove('show');
  document.querySelector('.backdrop').classList.remove('show');
}

function showOverlay(type) {
  // Hide any existing overlays first
  document.querySelectorAll('.howtoplay, .settings').forEach(el => {
    el.classList.remove('show');
  });
  
  // Show the requested overlay
  document.querySelector('.overlay').classList.add('show');
  document.querySelector(`.${type}`).classList.add('show');
}

function hideOverlay() {
  document.querySelector('.overlay').classList.remove('show');
  document.querySelectorAll('.howtoplay, .settings').forEach(el => {
    el.classList.remove('show');
  });
}

function hideAll() {
  hideModal();
  hideOverlay();
}

function onClickPlayAgain() {
  startNewGame();
}

// Global function for share button (called from innerHTML)
window.shareResults = shareResults;

// Initialize when DOM is loaded
document.addEventListener('DOMContentLoaded', onInit);