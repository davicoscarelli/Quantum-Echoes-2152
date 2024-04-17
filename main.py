import cv2
import pygame
import numpy as np
from pygame.locals import *
from app import App  # Make sure to import the App class
from icon import Icon
from pyswip import Prolog


# Initialize Pygame
pygame.init()

prolog = Prolog()

prolog.consult('mystery_knowledge_base.pl')

# Video setup
video_path = 'backdrop.mov'  # Replace with your video file path
cap = cv2.VideoCapture(video_path)
video_fps = cap.get(cv2.CAP_PROP_FPS)

# Window setup
window_size = (int(cap.get(cv2.CAP_PROP_FRAME_WIDTH)), int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT)))
window = pygame.display.set_mode(window_size)

# PNG overlay setup
png_path = 'background_game.png'  # Replace with your PNG file path
overlay_image = pygame.image.load(png_path).convert_alpha()

# Get the size of the PNG image
png_width, png_height = overlay_image.get_size()
# Resize the image to half its size
overlay_image = pygame.transform.scale(overlay_image, (png_width - (png_width // 3), png_height - (png_height // 3)))

# Window setup
window_size = (png_width - (png_width // 3), png_height - (png_height // 3))
window = pygame.display.set_mode(window_size)


# Calculate the new video size (about a third of the window size) and position
video_width = window_size[0] - 200
video_height = int((window_size[1] / window_size[0]) * video_width)
video_x = (window_size[0] - video_width + 20) // 2  # Center the video on the x-axis

# Define the simulated OS screen area (adjust as needed)
screen_rect = pygame.Rect(290, 158, window_size[0] - 578, window_size[1] - 345)
screen_color = (20, 20, 20)  # Dark grey for the OS screen

# Load custom cursor image
cursor_image_path = 'cursor.png'  # Replace with your cursor PNG file path
cursor_image = pygame.image.load(cursor_image_path).convert_alpha()

cursor_width = cursor_image.get_width() // 3
cursor_height = cursor_image.get_height() // 3
cursor_image = pygame.transform.scale(cursor_image, (cursor_width, cursor_height))

# Hide the system mouse cursor
pygame.mouse.set_visible(False)


app_window_image = pygame.image.load('window.png').convert_alpha()

# # Initialize the App window (assuming 'window.png' is the app window image)
# app_instance = App(app_window_image, (290, 158), window, screen_rect)

# # Define a list to hold icons
# icons = []

# # Create icons and add them to the list
# icons.append(Icon('notes.png', app_instance, (50, 50), window, screen_rect, 'App Name'))


# List of app definitions
apps = [
    {'icon_path': 'notes.png', 'name': 'ComLink'},
    {'icon_path': 'deduction_engine.png', 'name': 'Deduction Engine'},
    {'icon_path': 'terminal.png', 'name': 'Terminal'},
    {'icon_path': 'search.png', 'name': 'DataSleuth'},
    # Add more apps as needed
]


# Define a list to hold icons and apps
icons = []
apps_instances = []

icon_start_x = screen_rect.left + 10  # Start 10 pixels inside the screen_rect on the left
icon_start_y = screen_rect.top + 10   # Start 10 pixels inside the screen_rect on the top
icon_spacing = 100  # Space between icons
icon_row_capacity = screen_rect.width // icon_spacing  # Number of icons per row

# Initialize apps and icons
for index, app_def in enumerate(apps):
    position_x = icon_start_x + (index % icon_row_capacity) * icon_spacing
    position_y = icon_start_y + (index // icon_row_capacity) * icon_spacing

    app_instance = App(pygame.image.load('window.png'), (290, 158), window, screen_rect, app_def['name'], prolog)
    app_instance.visible = False  # Initially, app windows are not visible
    apps_instances.append(app_instance)
    
    icon_instance = Icon(app_def['icon_path'], app_instance, (position_x, position_y), window, screen_rect, app_def['name'])
    icons.append(icon_instance)



# Main loop control
running = True
clock = pygame.time.Clock()


while running:
    ret, frame = cap.read()
    if not ret:
        # Rewind the video file if we reached the end
        cap.set(cv2.CAP_PROP_POS_FRAMES, 0)
        continue

    frame = cv2.resize(frame, (video_width, video_height), interpolation=cv2.INTER_AREA)

    # Convert the frame to RGB, which Pygame uses
    frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)
    frame = np.rot90(frame)
    frame = pygame.surfarray.make_surface(frame)


    
    # Handle window events
    for event in pygame.event.get():
        if event.type == QUIT:
            running = False
        # Event handling for each app instance
        for app_instance in apps_instances:
            if app_instance.visible:  # Only handle events if the app is visible
                action = app_instance.handle_event(event)
                if action == 'close':
                    app_instance.close()
        
        for icon in icons:
            icon.handle_event(event)


    # Get current mouse position
    mouse_x, mouse_y = pygame.mouse.get_pos()

    # Constrain the mouse within the rectangle
    if not screen_rect.collidepoint(mouse_x, mouse_y):
        # Keep mouse within the screen rectangle
        new_mouse_x = min(max(mouse_x, screen_rect.left), screen_rect.right - cursor_image.get_width())
        new_mouse_y = min(max(mouse_y, screen_rect.top), screen_rect.bottom - cursor_image.get_height())
        pygame.mouse.set_pos(new_mouse_x, new_mouse_y)
        mouse_x, mouse_y = new_mouse_x, new_mouse_y  # Update position

    # Blit the video frame
    window.blit(frame, (video_x, 0))

    # Blit the PNG overlay
    window.blit(overlay_image, (0, 0))

    # Draw the simulated OS screen area on top
    pygame.draw.rect(window, screen_color, screen_rect)

    for icon in icons:
        icon.draw()

    for app in apps_instances:
        if app.visible:
            app.draw()

    # Draw the custom cursor
    window.blit(cursor_image, (mouse_x, mouse_y))

    # Refresh the display
    pygame.display.flip()

    # Tick the clock to match the video's framerate
    clock.tick(video_fps)

# Cleanup
cap.release()
pygame.quit()
