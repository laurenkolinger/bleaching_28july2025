#!/usr/bin/env python3
"""
Simple runner script for coral bleaching analysis

This script imports and runs the complete coral bleaching recovery analysis.
"""

import sys
import os

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from coral_bleaching_analysis import CoralBleachingAnalyzer

def main():
    """Run the coral bleaching analysis"""
    print("🌊 Coral Bleaching Recovery Analysis 🌊")
    print("Starting comprehensive analysis...")
    
    analyzer = CoralBleachingAnalyzer()
    analyzer.run_complete_analysis()

if __name__ == "__main__":
    main()