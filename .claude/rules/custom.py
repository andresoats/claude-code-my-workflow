import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib import font_manager

# Set up Palatino font (adjust path for your system)
# For macOS:
try:
    font_manager.fontManager.addfont('/System/Library/Fonts/Palatino.ttc')
    plt.rcParams['font.family'] = 'Palatino'
except:
    print("Palatino font not found, using default font")

# Base theme settings
def set_theme_custom(legend_position='top'):
    """
    Custom minimal theme equivalent to theme_custom in R
    
    Parameters:
    -----------
    legend_position : str
        'top', 'right', 'bottom', 'left', or 'none'
    """
    sns.set_style("whitegrid", {
        'axes.edgecolor': 'white',
        'grid.color': '.9',
        'axes.facecolor': 'white'
    })
    
    plt.rcParams.update({
        # Font settings
        'font.family': 'Palatino',
        'font.size': 10,
        
        # Title settings
        'axes.titlesize': 10,
        'axes.titleweight': 'normal',
        'axes.titlelocation': 'center',
        
        # Axis labels
        'axes.labelsize': 12,
        
        # Legend settings
        'legend.fontsize': 10,
        'legend.title_fontsize': 10,
        'legend.frameon': False,
        
        # Tick settings
        'xtick.labelsize': 10,
        'ytick.labelsize': 10,
        
        # Figure settings
        'figure.titlesize': 10,
        'figure.titleweight': 'normal',
        
        # Grid
        'axes.grid': True,
        'grid.alpha': 0.3,
    })
    
    # Set legend position
    if legend_position == 'none':
        plt.rcParams['legend.loc'] = 'upper right'  # default, but we'll hide it
    elif legend_position == 'top':
        plt.rcParams['legend.loc'] = 'upper center'
    elif legend_position == 'right':
        plt.rcParams['legend.loc'] = 'center left'
    else:
        plt.rcParams['legend.loc'] = legend_position


def set_theme_custom_bw(legend_position='top'):
    """
    Custom black & white theme equivalent to theme_custom_bw in R
    """
    sns.set_style("darkgrid", {
        'axes.edgecolor': 'black',
        'axes.linewidth': 0.8,
        'grid.color': '.8',
        'axes.facecolor': 'white'
    })
    
    plt.rcParams.update({
        'font.family': 'Palatino',
        'font.size': 10,
        'axes.titlesize': 10,
        'axes.titleweight': 'normal',
        'axes.titlelocation': 'center',
        'axes.labelsize': 12,
        'legend.fontsize': 10,
        'legend.title_fontsize': 10,
        'legend.frameon': True,
        'legend.fancybox': False,
        'legend.shadow': False,
        'xtick.labelsize': 10,
        'ytick.labelsize': 10,
        'figure.titlesize': 10,
        'axes.grid': True,
    })
    
    if legend_position == 'none':
        plt.rcParams['legend.loc'] = 'upper right'
    elif legend_position == 'top':
        plt.rcParams['legend.loc'] = 'upper center'
    elif legend_position == 'right':
        plt.rcParams['legend.loc'] = 'center left'
    else:
        plt.rcParams['legend.loc'] = legend_position


# Convenience functions for specific legend positions
def theme_custom():
    """Standard custom theme with legend on top"""
    set_theme_custom(legend_position='top')


def theme_custom_r():
    """Custom theme with legend on right"""
    set_theme_custom(legend_position='right')


def theme_custom_n():
    """Custom theme with no legend"""
    set_theme_custom(legend_position='none')


def theme_standard():
    """Alias for theme_custom"""
    theme_custom()


def apply_legend_position(ax, position='top'):
    """
    Apply legend position to a specific axes object
    
    Parameters:
    -----------
    ax : matplotlib axes
        The axes object to modify
    position : str
        'top', 'right', 'bottom', 'left', or 'none'
    """
    if position == 'none':
        legend = ax.get_legend()
        if legend:
            legend.remove()
    elif position == 'top':
        ax.legend(loc='upper center', bbox_to_anchor=(0.5, 1.15), 
                 ncol=3, frameon=False)
    elif position == 'right':
        ax.legend(loc='center left', bbox_to_anchor=(1, 0.5), 
                 frameon=False)
    elif position == 'bottom':
        ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.15), 
                 ncol=3, frameon=False)
    elif position == 'left':
        ax.legend(loc='center right', bbox_to_anchor=(0, 0.5), 
                 frameon=False)


# Custom color palettes
colours_custom_1_len6 = ["#C3AE78", "#026795", "#25980D", 
                         "#F77F00", "#344D33", "#616161"]

colours_custom_2_len8 = ["#2E5266", "#E63946", "#F77F00", "#6B9BD1", 
                         "#665191", "#457B9D", "#828282", "#FCBF49"]


# Example usage:
if __name__ == "__main__":
    import numpy as np
    
    # Set the theme
    theme_custom()
    
    # Create sample plot
    fig, ax = plt.subplots(figsize=(8, 6))
    
    x = np.linspace(0, 10, 100)
    for i, color in enumerate(colours_custom_1_len6[:3]):
        ax.plot(x, np.sin(x + i), label=f'Series {i+1}', color=color, linewidth=2)
    
    ax.set_xlabel('X Variable')
    ax.set_ylabel('Y Variable')
    ax.set_title('Example Plot with Custom Theme')
    
    # Apply legend position
    apply_legend_position(ax, position='top')
    
    plt.tight_layout()
    plt.show()