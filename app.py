import pygame
from app_content import TerminalApp, DeductionEngineApp, DataSleuthApp, ComLinkApp, NotesApp 

class App:
    def __init__(self, image, position, screen, os_screen_rect, name, prolog, app_manager):
        self.prolog = prolog
        self.app_manager = app_manager
        self.image = image
        self.rect = self.image.get_rect(topleft=position)
        self.screen = screen
        self.os_screen_rect = os_screen_rect
        self.name = name
        self.content = self.create_content(name)
        self.dragging = False
        self.drag_offset = (0, 0)
        self.title_bar_height = 20
        self.close_button_rect = pygame.Rect(self.rect.x, self.rect.y, 10, 10)
        self.visible = False

    def create_content(self, app_name):
        content_mapping = {
            'ComLink': ComLinkApp,
            'Terminal': TerminalApp,
            'Deduction Engine': DeductionEngineApp,
            'DataSleuth': DataSleuthApp,
            'NotesApp': NotesApp,
        }
        content_class = content_mapping.get(app_name)
        if content_class:
            return content_class(self.screen, self.rect, self.prolog, self.app_manager)
        else:
            raise ValueError(f"Unsupported app type: {app_name}")

    def handle_event(self, event):
        if self.visible:
            if event.type == pygame.MOUSEBUTTONDOWN:
                if self.close_button_rect.collidepoint(event.pos):
                    return 'close'
                if self.rect.collidepoint(event.pos) and event.pos[1] < self.rect.top + self.title_bar_height:
                    self.dragging = True
                    mouse_x, mouse_y = event.pos
                    self.drag_offset = (mouse_x - self.rect.x, mouse_y - self.rect.y)
            if event.type == pygame.MOUSEBUTTONUP:
                self.dragging = False
            if event.type == pygame.MOUSEMOTION and self.dragging:
                mouse_x, mouse_y = event.pos
                new_x = mouse_x - self.drag_offset[0]
                new_y = mouse_y - self.drag_offset[1]
                new_rect = pygame.Rect(new_x, new_y, self.rect.width, self.rect.height)
                new_rect.clamp_ip(self.os_screen_rect)
                self.rect.topleft = new_rect.topleft
                self.content.update_position(new_rect)
                self.close_button_rect.topleft = (self.rect.left, self.rect.top)

        if self.visible:
            self.content.handle_event(event)
        return 'none'

    def draw(self):
        if self.visible:
            self.screen.blit(self.image, self.rect)
            font = pygame.font.Font(None, 12)
            text_surf = font.render(self.name, True, (255, 255, 255))
            text_rect = text_surf.get_rect(center=(self.rect.centerx, self.rect.top + 10))
            self.screen.blit(text_surf, text_rect)
            self.content.draw()

    def show(self):
        self.visible = True
        self.content.update_position(self.rect)

    def close(self):
        self.visible = False

class AppManager:
    def __init__(self, apps):
        self.apps = apps  # List of all app instances

    def get_app(self, app_name):
        # Iterate over the list of apps and return the app with the matching name
        for app in self.apps:
            if app.name == app_name:
                return app
        return None  # Return None if no app matches the name
    
    def open_app(self, app_to_open):
        # Close all apps except the one to be opened
        for app in self.apps:
            app.visible = False
        app_to_open.visible = True
        app_to_open.show()  # Ensure the app is properly positioned and shown
