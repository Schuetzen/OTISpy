import tkinter as tk

# Function placeholders for button commands
def calibrate():
    pass  # Placeholder for calibrate function

def run_plot():
    pass  # Placeholder for run and plot function

# Create the main application window
root = tk.Tk()
root.title('Enhanced OTIS model')

# Create Frames for layout
left_frame = tk.Frame(root, padx=10, pady=10)
right_frame = tk.Frame(root, padx=10, pady=10)
left_frame.grid(row=0, column=0, sticky='nswe')
right_frame.grid(row=0, column=1, sticky='nswe')

# Left frame entries and labels
entries = {}
labels = [
    'Obv data Location (m) or Time (s)', 'Distance step (m)', 'Time step (s)',
    'Run duration (s)', 'Injection mass (g)', 'Injection volume (L)',
    'Injection duration (s)', 'Maximum iterations'
]

for i, label in enumerate(labels):
    tk.Label(left_frame, text=label).grid(row=i, column=0, sticky='e')
    entry = tk.Entry(left_frame)
    entry.grid(row=i, column=1, sticky='ew')
    entries[label] = entry

# Right frame - Run calibrated Model
run_frame = tk.LabelFrame(right_frame, text='Run calibrated Model')
run_frame.pack(fill='both', expand=True)

run_labels = [
    'Model Type', 'Tracer Type', 'Obsv data type', 'Plot Type', 'At time (s)', 'At location (m)', 'Filename.xlsx'
]

# Create labels and entries for run and plot parameters
for i, text in enumerate(run_labels):
    tk.Label(run_frame, text=text).grid(row=i, column=0, sticky='e')
    if text in ['Model Type', 'Tracer Type', 'Obsv data type', 'Plot Type']:
        combobox = tk.Entry(run_frame)
        combobox.grid(row=i, column=1, sticky='ew')
        entries[text] = combobox
    else:
        entry = tk.Entry(run_frame)
        entry.grid(row=i, column=1, sticky='ew')
        entries[text] = entry

# Calibrate and Run & Plot buttons
calibrate_button = tk.Button(run_frame, text='Calibrate', command=calibrate)
calibrate_button.grid(row=4, column=1, sticky='ew')
run_button = tk.Button(run_frame, text='Run & Plot', command=run_plot)
run_button.grid(row=7, column=1, sticky='ew')

# Make the rows and columns adjust to the window size
for i in range(len(labels)):
    left_frame.grid_rowconfigure(i, weight=1)
    left_frame.grid_columnconfigure(i, weight=1)

# Start the application
root.mainloop()
