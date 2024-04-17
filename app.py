import pygame
from app_content import NotesApp, DeductionEngineApp, DataSleuthApp, ComLinkApp  # Assuming TerminalApp is also defined

class App:
    def __init__(self, image, position, screen, os_screen_rect, name, prolog):
        self.prolog = prolog
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
            'Terminal': NotesApp,
            'Deduction Engine': DeductionEngineApp,
            'DataSleuth': DataSleuthApp,
        }
        content_class = content_mapping.get(app_name)
        if content_class:
            return content_class(self.screen, self.rect, self.prolog)
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
