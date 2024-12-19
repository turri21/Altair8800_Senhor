import serial
import threading
import tkinter as tk
from tkinter import ttk
import time
from datetime import datetime
from collections import deque
import argparse

class SerialTerminal:
    def __init__(self, port='COM9', baudrate=19200):
        self.serial = serial.Serial(
            port=port,
            baudrate=baudrate,
            bytesize=serial.EIGHTBITS,
            parity=serial.PARITY_NONE,
            stopbits=serial.STOPBITS_ONE,
            timeout=0.1
        )
        self.running = False
        self.last_sent = deque(maxlen=8)
        self.last_received = deque(maxlen=8)
        
        # Create main window
        self.root = tk.Tk()
        self.root.title(f"Serial Terminal - {port}")
        self.root.geometry("800x600")
        
        # Create terminal display
        self.display = tk.Text(self.root, bg='black', fg='light green', 
                             font=('Courier', 12), width=80, height=24)
        self.display.pack(fill=tk.BOTH, expand=True)
        
        # Create debug line
        self.debug_var = tk.StringVar()
        self.debug_label = ttk.Label(self.root, textvariable=self.debug_var,
                                   font=('Courier', 10), foreground='blue')
        self.debug_label.pack(fill=tk.X)
        
        # Create status bar
        self.status_var = tk.StringVar()
        self.status_bar = ttk.Label(self.root, textvariable=self.status_var, 
                                  font=('Courier', 10), relief=tk.SUNKEN)
        self.status_bar.pack(fill=tk.X)
        
        # Bind keyboard events
        self.root.bind('<Key>', self.handle_keypress)
        self.root.bind('<Escape>', lambda e: self.cleanup())
        
        # Configure text widget
        self.display.configure(insertbackground='light green')
        
    def format_hex(self, char_list):
        return ' '.join([f"{ord(c):02X}" if isinstance(c, str) else f"{c:02X}" for c in char_list])
        
    def update_status(self):
        sent_hex = self.format_hex(self.last_sent)
        received_hex = self.format_hex(self.last_received)
        status = f"TX:[{sent_hex:24}] RX:[{received_hex:24}] | {self.serial.port}@{self.serial.baudrate} | ESC=exit"
        self.status_var.set(status)

    def show_debug(self, msg):
        self.debug_var.set(msg)
        self.root.update_idletasks()

    def handle_keypress(self, event):
        if not self.running or not event.char:
            return

        # Convert character to its raw value
        raw_value = ord(event.char)

        # Handle special keys
        if event.keysym == 'Return':
            raw_value = 13  # CR
        elif event.keysym == 'BackSpace':
            raw_value = 8   # BS
            
        # Only process printable chars and control chars we care about
        if raw_value in [8, 13] or 32 <= raw_value <= 126:
            self.show_debug(f"Key pressed: hex={raw_value:02X}")
            self.serial.write(bytes([raw_value]))
            self.serial.flush()
            self.last_sent.append(raw_value)
            self.update_status()

    def read_serial(self):
        while self.running:
            if self.serial.in_waiting:
                try:
                    data = self.serial.read()
                    self.last_received.append(data[0])
                    
                    # Enable text widget temporarily
                    self.display.config(state='normal')
                    
                    # Display the received character
                    char = data.decode('ascii', errors='replace')
                    if char == '\r' or char == '\n':
                        # Display CR and LF exactly as received
                        self.display.insert(tk.END, char)
                    elif char == '\b':
                        # Handle backspace from device
                        current = self.display.get("end-2c", "end-1c")
                        if current != '\n':
                            self.display.delete("end-2c", "end-1c")
                    else:
                        self.display.insert(tk.END, char)
                    
                    # Disable text widget again
                    self.display.config(state='disabled')
                    
                    self.display.see(tk.END)
                    self.update_status()
                    
                except Exception as e:
                    self.show_debug(f"Error reading: {e}")
            time.sleep(0.01)

    def run(self):
        try:
            self.running = True
            self.show_debug(f"Terminal started on {self.serial.port}")
            
            # Disable text widget
            self.display.config(state='disabled')
            
            # Start the serial reading thread
            read_thread = threading.Thread(target=self.read_serial)
            read_thread.daemon = True
            read_thread.start()

            # Start the Tkinter event loop
            self.root.mainloop()

        except Exception as e:
            self.show_debug(f"Fatal error: {e}")
        finally:
            self.cleanup()

    def cleanup(self):
        self.running = False
        if self.serial.is_open:
            self.serial.close()
        self.root.destroy()
            
def main():
    # Parse command line arguments
    parser = argparse.ArgumentParser(description='Serial Terminal for Altair8800_MiSTer')
    parser.add_argument('-p', '--port', default='COM9',
                        help='Serial port to use (default: COM9)')
    args = parser.parse_args()
    
    try:
        terminal = SerialTerminal(port=args.port)
        terminal.run()
    except Exception as e:
        print(f"Fatal error: {e}")

if __name__ == "__main__":
    main()