# Cat & Mouse Game - AI Planning Project

A text-based adventure game built with SWI-Prolog, featuring PDDL-based opponent AI and optional web visualization.

---
## Author Information

- **Course:** JC4002 - Knowledge Representation
- **Assignment:** Text-based Prolog adventure game with PDDL opponent
- **Academic Year:** 2025-2026
- **Authors:** Minzhen Lai (50087369) & Ziyang Lu (50087371)
- **Contact:** If you encounter any issues running this game, please feel free to contact: u15zl22@abdn.ac.uk

---

## Project Overview

**Game Concept:**  
You play as a mouse trying to find a cookie in the kitchen while avoiding a cat (AI opponent) and mousetraps. Features:
- Three rooms: Bedroom, Kitchen, and Living Room
- Two locked doors requiring keys
- Randomly placed items and traps
- PDDL-based planner controlling the cat's behavior
- Fog of war exploration system

**Core Features:**
- Text-based Prolog game (core requirement)
- PDDL-based opponent AI (using Pyperplan)
- Dynamic knowledge base with randomization
- Win/lose conditions and game state management
- **Extended features:** Web-based visualization and HTTP server

---

## File Structure

```
CI/
├── Core game files (Required)
│   ├── cat_mouse.pl                    # Main game logic (716 lines)
│   ├── catmouse-domain.pddl            # PDDL domain definition
│   ├── catmouse-problem-full.pddl      # PDDL problem template
│   └── pyperplan_runner.pl             # Pyperplan integration module
│
├── Extended features (Additional)
│   ├── game_server.pl                  # HTTP server for web frontend
│   ├── game_frontend_connected.html    # Web visualization interface
│   └── start_server.bat                # Server quick start script
│
└── Documentation
    ├── README.md                       # English documentation
    └── README_CN.md                    # Chinese documentation
```

---

## Prerequisites

### Required Software:
1. **SWI-Prolog** (version 9.x or higher)
   - Download from: https://www.swi-prolog.org/Download.html
   - Ensure it's added to your system PATH

2. **Pyperplan** (STRIPS planner)
   - The code expects: `C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe`
   - **If your path differs, see "Path Configuration" section below**

3. **Modern Web Browser** (for extended features)
   - Chrome, Firefox, or Edge

---

## Path Configuration (IMPORTANT!)

Before running the game, you **MUST** update the following paths to match your system:

### File: `cat_mouse.pl`

**Line 13** - Module import path:
```prolog
:- use_module('C:/Users/ziyou/Desktop/CI/pyperplan_runner.pl').
```
Change `C:/Users/ziyou/Desktop/CI/` to **your project directory**.

**Line 17** - Pyperplan executable path:
```prolog
Exe = 'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
Change to **your pyperplan.exe location**.

**Line 625** - Pyperplan executable path (in test_plan):
```prolog
'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
Change to **your pyperplan.exe location**.

**Line 626** - PDDL domain file path (in test_plan):
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
```
Change to **your project directory**.

**Line 627** - PDDL problem file path (in test_plan):
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
```
Change to **your project directory**.

**Line 650** - PDDL problem generation path (in cat_turn):
```prolog
generate_pddl_problem('C:/Users/ziyou/Desktop/CI/catmouse-current.pddl'),
```
Change to **your project directory**.

**Line 654** - Pyperplan executable path (in cat_turn):
```prolog
'C:/Users/ziyou/miniconda3/Scripts/pyperplan.exe',
```
Change to **your pyperplan.exe location**.

**Line 655** - PDDL domain file path (in cat_turn):
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-domain.pddl',
```
Change to **your project directory**.

**Line 656** - PDDL problem file path (in cat_turn):
```prolog
'C:/Users/ziyou/Desktop/CI/catmouse-current.pddl',
```
Change to **your project directory**.

### Quick Find & Replace:

If your project is in a different directory, you can use your editor's find-and-replace:
- **Find:** `C:/Users/ziyou/Desktop/CI/`
- **Replace with:** `YOUR_PROJECT_PATH/`

**Example:**
- If your project is at `D:/MyProjects/CatMouse/`
- Replace all occurrences with `D:/MyProjects/CatMouse/`

### Note about Pyperplan:
- **Three locations** need pyperplan.exe path: Line 17, Line 625, Line 654
- Recommend using the same path for all three

---

## Running the Game

### Method 1: Text-Based Console Game (Core Requirement)

This is the primary way to run the game as specified in the assignment.

**Step 1:** Open SWI-Prolog

**Step 2:** Load the game
```prolog
?- cd('YOUR_PROJECT_PATH/').
?- [cat_mouse].
```

**Step 3:** Start the game
```prolog
?- start.
```

**Step 4:** Play the game using commands:
```prolog
?- look.                    % Describe current location
?- sense.                   % Sense adjacent cells
?- move(north).             % Move (north/south/east/west)
?- take(bedroom_key).       % Pick up an item
?- unlock(bedroom_door).    % Unlock a door (must stand on it)
?- status.                  % Show current game state
?- cat_turn.                % Cat's turn (AI planning)
```

**Example Game Session:**
```prolog
?- start.
Welcome to Cat & Mouse!
You are at (1,8) in the living_room.
This is your cosy mouse hole.

?- sense.
You sniff around...
To the north: empty floor.
To the south: the outer wall.
To the west: the outer wall.
To the east: empty floor.

?- move(north).
You move to (1,7).
You are at (1,7) in the living_room.

?- cat_turn.
Cat is at (1,1), mouse at (1,7).
Cat planned first action: move_cat(c1_1,c1_2)
The cat moves to (1,2).

... continue playing ...
```

---

### Method 2: Web-Based Interface (Extended Feature)

This demonstrates the extended capabilities with HTTP server and web visualization.

**Step 1:** Start the Prolog server
```prolog
?- cd('YOUR_PROJECT_PATH/').
?- [cat_mouse].
?- [game_server].
?- start_server(9178).
```

You should see:
```
========================================
  Cat & Mouse Game Server Started
========================================
  URL: http://localhost:9178
  Log File: game_log.txt
========================================
```

**Step 2:** Open your web browser and visit:
```
http://localhost:9178
```

**To stop the server:**
```prolog
?- halt.
```
Or press `Ctrl+C` and then type `a` (abort).

**Step 3:** Play using the web interface:
- Use arrow keys or WASD to move
- Press Space to sense around
- Press E to take items
- Press F to unlock doors
- Click "Cat's Turn" for AI opponent move

**Features:**
- Real-time game visualization
- Fog of war system (explore as you go)
- Interactive controls (mouse + keyboard)
- Game log with timestamps
- Professional UI design

---

## Game Rules & Map

### Map Layout (5x8 grid):
```
Bedroom (1-3, 1-3)     Kitchen (4-5, 1-3)
  [bedroom_key]           [cookie]
       |                      |
  bedroom_door(3,4)      kitchen_door(5,4)
       |                      |
       +----------------------+
              |
    Living Room (1-5, 5-8)
  [kitchen_key] [traps x2]
  
  Mouse Hole (1,8) - Starting point
```

### Objective:
- **Win:** Get the cookie from the kitchen
- **Lose:** Get caught by the cat OR step on a mousetrap

### Strategy:
1. Start at mouse hole (1,8)
2. Use `sense` to explore safely
3. Find kitchen_key in living room
4. Unlock kitchen door (5,4)
5. Get the cookie from kitchen
6. Avoid the cat's AI planning!

---

## AI Planning System

The cat uses **PDDL planning** (Pyperplan with STRIPS) to chase the mouse intelligently:

1. **Dynamic Problem Generation:** Each turn, a new PDDL problem file is generated based on current game state
2. **Planning:** Pyperplan computes a plan for the cat to reach the mouse's position
3. **Execution:** The first action from the plan is executed
4. **Replanning:** The process repeats every turn (adaptive AI)

**PDDL Actions Available to Cat:**
- `move-cat`: Normal movement between adjacent cells
- `pickup-key`: Pick up keys from the floor
- `unlock-door`: Unlock doors with correct keys
- `move-through-door`: Move through unlocked doors

---

## Generating Transcript

To generate `transcript.txt` for submission, use SWI-Prolog's built-in `protocol/1`:

```prolog
?- protocol('transcript.txt').
?- [cat_mouse].
?- start.
... play the game ...
?- noprotocol.
```

This will record all console output to `transcript.txt`.

---

## Troubleshooting

### Issue 1: "Pyperplan not found"
**Solution:** Update the pyperplan path in `cat_mouse.pl` line 20:
```prolog
Exe = 'YOUR_PATH_TO/pyperplan.exe',
```

### Issue 2: "Module pyperplan_runner not found"
**Solution:** Ensure you're in the correct directory:
```prolog
?- pwd.
?- cd('C:/Users/ziyou/Desktop/CI').
```

### Issue 3: Web interface - HTTP 500 errors
**Solution:** Stop and restart the server:
```prolog
?- halt.
% Then restart server:
?- [game_server].
?- start_server(9178).
```

### Issue 4: Web interface - Cannot connect
**Solution:** 
1. Check server is running: `start_server(9178)` should show success message
2. Check browser console (F12) for errors
3. Try clearing browser cache: Ctrl+Shift+R

### Issue 5: Port 9178 already in use
**Solution:** Use a different port:
```prolog
?- start_server(8080).
```
Then update line 19 in `game_frontend_connected.html`:
```javascript
const API_BASE = 'http://localhost:8080/api';
```

---

## Game Log Monitoring

When running the web interface, all game actions are logged to `game_log.txt` with timestamps.

**To monitor in real-time (PowerShell):**
```powershell
Get-Content game_log.txt -Wait -Tail 50
```

**Log format:**
```
[23:25:01] [API] ========================================
[23:25:01] [API] Player command: move(east)
[23:25:01] [GAME] Mouse at (1,8)
[23:25:01] [GAME] Mouse moved to (2,8)
[23:25:15] [API] CAT AI TURN
[23:25:15] [GAME] Cat at (1,1), Mouse at (2,8)
[23:25:15] [GAME] Cat moved to (1,2)
```

---

## Technical Highlights

### Knowledge Representation:
- **Static facts:** `room_of/3`, `door_cell/3`, `wall/2`, `walkable/2`
- **Dynamic facts:** `at/3`, `item_at/3`, `has/2`, `trap/2`, `locked/1`, `turn/1`
- **Rules:** Movement validation, door mechanics, win/lose conditions

### PDDL Integration:
- Domain file defines cat's action space
- Problem file dynamically generated each turn
- Pyperplan computes optimal path to mouse
- First action from plan is executed

### Randomization:
- Bedroom key: Random placement in bedroom (excluding cat's start)
- Kitchen key: Random placement in living room
- Cookie: Random placement in kitchen
- Traps: 2 randomly placed in living room (safe zones excluded)

### Extended Features:
- HTTP server using `library(http/thread_httpd)`
- RESTful API with JSON responses
- Real-time game state synchronization
- Web-based visualization with fog of war
- Professional UI design (Fredoka font, gradient backgrounds, animations)

---

## Quick Start Guide (For Evaluation)

**For basic text-based gameplay:**
```prolog
?- [cat_mouse].
?- start.
```

**For full experience with web interface:**
```
1. Double-click start_server.bat
2. Open browser: http://localhost:9178
3. Play!
```

---

## Web Interface Controls

| Input | Action |
|-------|--------|
| Arrow keys / WASD | Move mouse |
| Space | Sense around |
| E | Take item |
| F | Unlock door |
| R | Restart game |

Or use the on-screen buttons.

---

## Assignment Compliance

This project fulfills all requirements:
- Text-based Prolog adventure game
- Knowledge base with facts and rules
- PDDL-based opponent AI (Pyperplan)
- Dynamic planning and replanning
- Randomness and uncertainty
- Win/lose conditions
- Web interface and HTTP server (suggested extension in assignment)

---

## Notes

- The game features a **fog of war** system where players must use `sense` to explore
- The cat uses **replanning** each turn based on mouse's current position
- Safe cells (1,7), (2,8), and (5,5) never have traps
- Getting the cookie instantly wins the game (no need to return home)
- All game output can be captured using `protocol/1` for transcript generation

---

## Quick Test

To verify everything works:

```prolog
?- [cat_mouse].
?- start.
?- sense.
?- move(east).
?- cat_turn.
```

You should see:
- Mouse movement confirmation
- Cat AI planning output showing PDDL actions
- Updated game state

Enjoy the game!

---

## Grading Criteria Alignment

### Knowledge Base (10 points)
**Facts and Data:**
- Static facts: Room definitions, map layout, door and wall positions
- Dynamic facts: Character positions, item locations, door lock status, turn number
- All facts are used in the game, no redundancy

**Dynamic Fact Management:**
- Uses `assert/1` and `retract/1` for dynamic updates
- All mutable predicates declared with `dynamic`
- Game state consistency validated through rules

### Knowledge Base Rules (20 points)
**Game Rule Implementation:**
- Movement rules: `move/1`, `can_pass/5`, `update_position/5`
- Interaction rules: `take/1`, `unlock/1`
- State checks: `check_game_over/0`, `check_not_over/0`

**Win/Lose Conditions:**
- Win: `has(mouse,cookie)` - Get the cookie
- Lose1: `at(mouse,X,Y), trap(X,Y)` - Step on trap
- Lose2: `at(mouse,X,Y), at(cat,X,Y)` - Caught by cat

**Opponent Decision Making:**
- Uses Pyperplan PDDL planner
- Dynamically generates problem file each turn
- Replans based on current state

**Randomness/Uncertainty:**
- Item placement randomized: `place_bedroom_key/0`, `place_kitchen_key/0`, `place_cookie/0`
- Trap placement randomized: `place_traps/0`
- Uses `random_member/2` and `random_permutation/2`

**Arithmetic and Data Structures:**
- Coordinate system: (X,Y) integer pairs
- Coordinate conversion: `coord_cell_name/3`, `cell_name_coord/3`
- Direction calculation: `step/5` predicate

### Planning Execution (15 points)
**Planning Execution Flow:**
1. `cat_turn/0` calls `generate_pddl_problem/1` to generate current state
2. Calls `pyperplan_solve/3` to get plan
3. `cat_apply_first_action/1` executes first action
4. `cat_do_action/1` parses and validates action

**Action Validation:**
- Checks door open/closed state
- Verifies key possession
- Confirms position validity
- Handles illegal actions (outputs error messages)

### Planning Dynamics (20 points)
**Dynamic Replanning:**
- Regenerates PDDL problem each turn
- Goal is always the mouse's current location
- Cat must adjust strategy based on door states
- Complete planning chain for picking keys, unlocking doors, moving

**Strategy Adaptation:**
- Cat's behavior based on real-time game state
- When doors are locked, must find keys first
- When doors are open, can choose shorter paths
- Fully automated AI decision-making

---

## Operation Guide

### Quick Feature Verification:

**1. Text Game (Core):**
```prolog
?- [cat_mouse].
?- start.
?- sense.          % Check knowledge base rules
?- move(north).    % Test movement rules
?- cat_turn.       % Verify PDDL planning
```

**2. Web Interface (Additional):**
- Double-click `start_server.bat`
- Visit `http://localhost:9178`
- Experience modern game interface

**3. View PDDL Planning Process:**
Running the game generates:
- `catmouse-current.pddl` - Current state problem file
- `catmouse-current.pddl.soln` - Planner's solution

**4. View Real-time Logs:**
Open `game_log.txt` to view detailed game operation logs

---

## Project Highlights

1. **Complete Knowledge Representation**
   - 76 static facts (map, rooms, doors)
   - 8 types of dynamic facts
   - 20+ rules

2. **Intelligent Opponent AI**
   - Uses STRIPS planner
   - Adaptive replanning each turn
   - Considers doors, keys, path optimization

3. **Rich Game Mechanics**
   - Fog of war system
   - Multi-level map
   - Item management system
   - Door lock mechanism

4. **Professional Extensions**
   - RESTful API design
   - Frontend-backend separation
   - Real-time state synchronization
   - Modern UI/UX design

---

## Contact

For technical issues, please check:
1. Pyperplan path is correct
2. SWI-Prolog version compatibility
3. Browser console error messages (F12)
4. game_log.txt log file

---
