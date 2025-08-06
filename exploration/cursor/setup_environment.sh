#!/bin/bash

# Coral Bleaching Analysis Environment Setup
# This script helps set up the Python environment for the coral bleaching analysis

echo "🐠 Setting up Coral Bleaching Analysis Environment 🐠"
echo "======================================================="

# Check if Python 3 is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is not installed. Please install Python 3 first."
    exit 1
fi

echo "✓ Python 3 found: $(python3 --version)"

# Function to try different installation methods
install_packages() {
    echo "📦 Installing required Python packages..."
    
    # Method 1: Try pip install with user flag
    echo "Trying pip install --user..."
    if python3 -m pip install --user pandas numpy matplotlib seaborn scipy scikit-learn jupyter; then
        echo "✓ Packages installed successfully with pip --user"
        return 0
    fi
    
    # Method 2: Try creating virtual environment
    echo "Trying virtual environment..."
    if python3 -m venv coral_env; then
        source coral_env/bin/activate
        if pip install pandas numpy matplotlib seaborn scipy scikit-learn jupyter; then
            echo "✓ Packages installed successfully in virtual environment"
            echo "ℹ️  To use the analysis, run: source coral_env/bin/activate"
            return 0
        fi
    fi
    
    # Method 3: Try system packages (Ubuntu/Debian)
    echo "Trying system packages..."
    if command -v apt &> /dev/null; then
        echo "ℹ️  You may need to run: sudo apt install python3-pandas python3-numpy python3-matplotlib python3-seaborn python3-scipy python3-sklearn python3-jupyter"
    fi
    
    # Method 4: Try with --break-system-packages (last resort)
    echo "Trying with --break-system-packages (last resort)..."
    if python3 -m pip install --break-system-packages pandas numpy matplotlib seaborn scipy scikit-learn jupyter; then
        echo "✓ Packages installed with --break-system-packages"
        echo "⚠️  Warning: This may affect your system Python installation"
        return 0
    fi
    
    return 1
}

# Try to install packages
if install_packages; then
    echo ""
    echo "🎉 Environment setup complete!"
    echo ""
    echo "You can now run the analysis:"
    echo "  python3 coral_bleaching_analysis.py      # Full analysis"
    echo "  python3 coral_analysis_simple.py         # Simplified analysis (no deps)"
    echo "  jupyter notebook coral_bleaching_exploration.ipynb  # Interactive analysis"
    echo ""
else
    echo ""
    echo "❌ Could not install all required packages automatically."
    echo ""
    echo "Manual installation options:"
    echo "1. Virtual environment:"
    echo "   python3 -m venv coral_env"
    echo "   source coral_env/bin/activate"
    echo "   pip install -r requirements.txt"
    echo ""
    echo "2. System packages (Ubuntu/Debian):"
    echo "   sudo apt install python3-pandas python3-numpy python3-matplotlib python3-seaborn python3-scipy python3-sklearn python3-jupyter"
    echo ""
    echo "3. Use simplified analysis (no dependencies required):"
    echo "   python3 coral_analysis_simple.py"
    echo ""
fi

# Test if we can import the main packages
echo "🧪 Testing package imports..."
python3 -c "
try:
    import pandas, numpy, matplotlib, seaborn, scipy, sklearn
    print('✓ All required packages are available')
except ImportError as e:
    print(f'❌ Missing package: {e}')
    print('ℹ️  You can still use the simplified analysis: python3 coral_analysis_simple.py')
"

echo ""
echo "Setup script complete. Happy analyzing! 🌊"