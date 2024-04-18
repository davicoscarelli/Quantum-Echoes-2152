# Quantum Echoes 2152

Quantum Echoes 2152 is an interactive detective game set in the year 2152, where players use a variety of applications within a simulated operating system to solve the mysterious disappearance of a quantum AI researcher.

## Setup Instructions

Follow these steps to set up your environment and run the game.

### Prerequisites

Ensure you have Python 3 installed on your machine. You will also need SWI-Prolog version 8.4.2, which can be downloaded from the [SWI-Prolog website](http://www.swi-prolog.org/Download.html).

### Environment Setup

1. **Create and activate a Python virtual environment:**
   ```bash
   python3 -m venv pyswip_env
   source pyswip_env/bin/activate
   ```

2. **Install SWI-Prolog:**
   After downloading, follow the installation instructions on their website. Make sure to adjust the paths in the next steps according to your installation.

3. **Set environment variables (Mac OS):**
   ```bash
   export DYLD_FALLBACK_LIBRARY_PATH=/Applications/SWI-Prolog.app/Contents/Frameworks
   export PATH=$PATH:/Applications/SWI-Prolog.app/Contents/MacOS
   ```

   For Windows or Linux, adjust these paths according to where SWI-Prolog is installed on your system.

4. **Install required Python packages:**
   ```bash
   pip3 install -r requirements.txt
   ```

### Running the Game

After setting up the environment and dependencies, run the game using:

```bash
python3 main.py
```

## Game Instructions

- Navigate through the simulated operating system using various applications to gather clues.
- Use the Deduction Engine to input evidence and deduce new information.
- Solve the mystery by piecing together the evidence collected throughout the game.