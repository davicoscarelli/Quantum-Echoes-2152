import pygame
import time

class Icon:
    ICON_SIZE = (40, 40)

    def __init__(self, image, app, position, screen, os_screen_rect, name):
        self.original_image = pygame.image.load(image).convert_alpha()
        self.image = pygame.transform.scale(self.original_image, self.ICON_SIZE)  # Resize icon image
        self.rect = self.image.get_rect(topleft=position)
        self.app = app
        self.screen = screen
        self.os_screen_rect = os_screen_rect
        self.name = name
        self.last_click_time = 0
    def handle_event(self, event):
        if event.type == pygame.MOUSEBUTTONDOWN:
            if self.rect.collidepoint(event.pos):
                # Check for double click
                if time.time() - self.last_click_time < 0.5:
                    self.app.visible = True  # Make the app window visible
                self.last_click_time = time.time()

    def draw(self):
        self.screen.blit(self.image, self.rect)
        # Draw the app name beneath the icon
        font = pygame.font.Font(None, 18)
        text_surf = font.render(self.name, True, (255, 255, 255))
        text_rect = text_surf.get_rect(center=(self.rect.centerx, self.rect.bottom + 10))
        self.screen.blit(text_surf, text_rect)
