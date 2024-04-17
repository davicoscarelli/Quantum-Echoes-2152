import pygame
from monoboxPygame import Textbox


class AppContent:
    def __init__(self, screen, rect, prolog):
        self.screen = screen
        self.rect = rect
        self.prolog = prolog

    def handle_event(self, event):
        raise NotImplementedError

    def draw(self):
        raise NotImplementedError

class NotesApp(AppContent):
    def __init__(self, screen, rect, prolog):
        super().__init__(screen, rect, prolog)
        # Initialize the textbox with margins
        self.initialize_textbox(rect)

    def initialize_textbox(self, rect):
        # Create a padded rectangle inside the provided rect
        padded_rect = pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 20, rect.height - 40)
        self.textbox = Textbox(rect=padded_rect, size=14)

    def update_position(self, new_rect):
        # Update the main rect and then adjust for internal padding
        self.rect = new_rect
        # Create a new padded rect for the textbox based on the new outer rect
        padded_rect = pygame.Rect(new_rect.x + 10, new_rect.y + 30, new_rect.width - 20, new_rect.height - 40)
        # Update textbox position and size
        self.textbox.move(padded_rect.x, padded_rect.y)
        self.textbox.resize(padded_rect.width, padded_rect.height)

    def handle_event(self, event):
        # Pass events to the textbox
        self.textbox.update([event])

    def draw(self):
        # Render the textbox
        self.textbox.render()

class Notification:
    def __init__(self, sender, message, screen, rect):
        self.sender = sender
        self.message = message
        self.screen = screen
        self.rect = rect  # Position and size of the notification

    def draw(self):
        pygame.draw.rect(self.screen, (100, 100, 100), self.rect)
        font = pygame.font.Font(None, 12)
        sender_surf = font.render(f"From: {self.sender}", True, (255, 255, 255))
        message_surf = font.render(self.message, True, (200, 200, 200))
        self.screen.blit(sender_surf, (self.rect.x + 5, self.rect.y + 5))
        self.screen.blit(message_surf, (self.rect.x + 5, self.rect.y + 15))

class ComLinkApp(AppContent):
    def __init__(self, screen, rect, prolog):
        super().__init__(screen, rect, prolog)
        self.notifications = []
        self.add_message("Partner", "Initial test message to check the functionality.")  # Adding an initial message for testing

    def update_position(self, new_rect):
        self.rect = new_rect
        y_offset = 20
        for notification in self.notifications:
            notification.rect = pygame.Rect(new_rect.x + 10, new_rect.y + y_offset, new_rect.width - 20, 30)
            y_offset += 35

    def handle_event(self, event):
        # This method is reserved for future use if needed
        pass

    def draw(self):
        for notification in self.notifications:
            notification.draw()

    def add_message(self, sender, message):
        new_rect = pygame.Rect(self.rect.x + 10, self.rect.y + 20 + len(self.notifications) * 35, self.rect.width - 20, 30)
        self.notifications.append(Notification(sender, message, self.screen, new_rect))
        # Automatically open the app window to show the new message
        self.visible = True

class DeductionEngineApp(AppContent):
    def __init__(self, screen, rect, prolog):
        super().__init__(screen, rect, prolog)
        self.prolog = prolog
        self.initialize_textbox(rect)

    def initialize_textbox(self, rect):
        # Create a padded rectangle inside the provided rect
        padded_rect = pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 20, rect.height - 40)
        self.textbox = Textbox(rect=padded_rect, size=14)
        self.inference_button = Button(self.screen, rect=pygame.Rect(rect.right - 80, rect.bottom - 30, 70, 20), label="Inference")

    def update_position(self, new_rect):
        # Update the main rect and adjust the internal components
        self.rect = new_rect
        # Adjust the textbox and button positions based on the new app window dimensions
        padded_rect = pygame.Rect(new_rect.x + 10, new_rect.y + 30, new_rect.width - 20, new_rect.height - 40)
        self.textbox.move(padded_rect.x, padded_rect.y)
        self.textbox.resize(padded_rect.width, padded_rect.height)
        self.inference_button.rect = pygame.Rect(new_rect.right - 80, new_rect.bottom - 30, 70, 20)

    def handle_event(self, event):
        self.textbox.update([event])  # Update the textbox with user input
        if event.type == pygame.MOUSEBUTTONDOWN:
            if self.inference_button.rect.collidepoint(event.pos):
                self.run_inference()

    def draw(self):
        self.textbox.render()
        self.inference_button.draw()

    def run_inference(self):
        clues_text = self.textbox.toString()  # Correct method to fetch text from the textbox
        if clues_text:
            # Split and clean clues, ensuring they end without extra punctuation
            clues_list = [clue.strip().rstrip('.') for clue in clues_text.split('\n') if clue.strip()]

            print("Clues to add:", clues_list)

            try:
                self.clear_and_assert(clues_list)
                
                print("Checking suspects and their evidence requirements:")
                suspects = list(self.prolog.query("find_suspects(Suspect, Request)"))

                for suspect in suspects:
                    print(f"Suspect: {suspect['Suspect']}, Request: {suspect['Request']}")

                # Call show_evidence and check_requirements
                for answer in self.prolog.query("show_evidence(alex)"):
                    print(answer)
                for answer in self.prolog.query("check_requirements(alex)"):
                    print(answer)

                # Attempt to solve the mystery
                mastermind = list(self.prolog.query("deduce_mastermind(Suspect, Details)"))
                if mastermind:
                    print("Mastermind deduced:", mastermind)
                else:
                    print("Insufficient evidence to deduce the mastermind.")
            except Exception as e:
                print("Error interacting with Prolog:", str(e))
        else:
            print("No clues entered.")

    def clear_and_assert(self, clues_list):
        # Clear existing facts from the knowledge base
        clear_queries = [
            "retractall(lab_access(_, _))",
            "retractall(background(_, _))",
            "retractall(evidence(_, _, _))"
        ]
        for query in clear_queries:
            list(self.prolog.query(query))  # Fully consume the generator to ensure facts are retracted

        # Assert new clues to Prolog and fully consume the generator
        for clue in clues_list:
            query = f"assertz({clue})"
            list(self.prolog.query(query))  # Ensure the query is fully consumed
            print(f"Asserted to Prolog: {clue}")

    def print_knowledge_base(self):
        try:
            print("Current state of the knowledge base:")
            for result in self.prolog.query("list_facts"):
                print(result)
        except Exception as e:
            print(f"Error printing knowledge base: {e}")


class DataSleuthApp(AppContent):
    def __init__(self, screen, rect, prolog):
        super().__init__(screen, rect, prolog)
        self.text_input = Textbox(rect=pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 120, 20), size=14)
        self.search_button = Button(screen, rect=pygame.Rect(rect.right - 110, rect.y + 30, 100, 25), label="Search")
        self.results = []  # This will hold SearchResult instances
        self.showing_details = False
        self.close_button = Button(screen, rect=pygame.Rect(rect.left + 5, rect.y + 15, 15, 15), label='<')


    def update_position(self, new_rect):
        # Update the main rect and adjust for internal components
        self.rect = new_rect
        text_input_rect = pygame.Rect(new_rect.x + 10, new_rect.y + 30, new_rect.width - 120, 20)
        self.text_input.move(text_input_rect.x, text_input_rect.y)
        self.text_input.resize(text_input_rect.width, text_input_rect.height)
        self.search_button.rect = pygame.Rect(new_rect.right - 110, new_rect.y + 28, 100, 25)
        self.close_button.rect = pygame.Rect(new_rect.left + 5, new_rect.y + + 15, 15, 15)

        # Update each SearchResult's position
        y_offset = 60  # Increase starting y offset for results, adding more space
        for result in self.results:
            result.rect = pygame.Rect(new_rect.x + 10, new_rect.y + 10, new_rect.width - 20, 30)
            y_offset += 40  # Increase y offset for the next result for more space

    def handle_event(self, event):
        if self.showing_details:
            if event.type == pygame.MOUSEBUTTONDOWN and self.close_button.rect.collidepoint(event.pos):
                self.showing_details = False
        else:
            self.text_input.update([event])
            if event.type == pygame.MOUSEBUTTONDOWN:
                if self.search_button.rect.collidepoint(event.pos):
                    self.run_search(self.text_input.toString())
                elif len(self.results) > 0:
                    for result in self.results:
                        if result.rect.collidepoint(event.pos):
                            self.detail_title = result.title
                            self.detail_content = result.content
                            self.showing_details = True

    def draw(self):
        if self.showing_details:
            font = pygame.font.Font(None, 16)
            title_surf = font.render(self.detail_title, True, (255, 255, 255))
            self.screen.blit(title_surf, (self.rect.x + 10, self.rect.y + 50))
            font = pygame.font.Font(None, 12)
            content_surf = font.render(self.detail_content, True, (255, 255, 255))
            self.screen.blit(content_surf, (self.rect.x + 10, self.rect.y + 100))
            self.close_button.draw()
        else:
            self.text_input.render()
            self.search_button.draw()
            y_offset = 60  # Starting y offset for results
            for result in self.results:
                result.rect.y = self.rect.y + y_offset
                result.draw()
                y_offset += 40   # Adjust y offset for the next result

    def run_search(self, query):
        query = query.strip()  # Only strip whitespace, keep original casing if necessary
        print(f"Search Query: {query}")
        self.results.clear()  # Clear existing results

        query_result = self.prolog.query(f"search('{query}', Result)")
        try:
            for result in query_result:
                result_data = result["Result"]
                print("Raw result:", result_data)  # Debug print to see raw output from Prolog

                # Since result_data is a string, we need to parse it manually
                if "person(" in result_data:
                    # Extract data for a person
                    # Example person string: "person(Dr. Alice Smith, Tech Innovations Lab, [tags])"
                    parts = result_data[result_data.find("(")+1:result_data.rfind(")")].split(", ", 2)
                    name = parts[0]
                    affiliation = parts[1]
                    self.results.append(SearchResult(name, f"Works at {affiliation}", self.screen, pygame.Rect(self.rect.x + 10, self.rect.y + 10, self.rect.width - 20, 30)))
                elif "article(" in result_data:
                    # Extract data for an article
                    # Example article string: "article(AI Ethics Debate, The recent debate on AI ethics...)"
                    parts = result_data[result_data.find("(")+1:result_data.rfind(")")].split(", ", 1)
                    title = parts[0]
                    content = parts[1]
                    self.results.append(SearchResult(title, content, self.screen, pygame.Rect(self.rect.x + 10, self.rect.y + 10, self.rect.width - 20, 30)))
        finally:
            query_result.close()  # Make sure to close the query to free resources

        print(f"Search results for '{query}': {len(self.results)} found")
        for res in self.results:
            print("Added Result:", res.title)




class SearchResult:
    def __init__(self, title, content, screen, rect):
        self.title = title
        self.content = self.format_content(content)  # Format content for display
        self.screen = screen
        self.rect = rect

    def draw(self):
        pygame.draw.rect(self.screen, (100, 100, 100), self.rect)  # Background for the result
        font = pygame.font.Font(None, 20)
        title_surf = font.render(self.title, True, (255, 255, 255))
        content_surf = font.render(self.content, True, (200, 200, 200))
        self.screen.blit(title_surf, (self.rect.x + 5, self.rect.y + 1))
        self.screen.blit(content_surf, (self.rect.x + 5, self.rect.y + 16))

    def format_content(self, content):
        """ Truncate content to 30 characters, appending '...' if longer. """
        if len(content) > 30:
            return content[:28] + '...'
        return content

class DetailWindow:
    def __init__(self, title, content, screen):
        self.title = title
        self.content = content
        self.screen = screen
        self.rect = pygame.Rect(100, 100, 300, 200)  # Example dimensions
        self.visible = True

    def draw(self):
        if not self.visible:
            return
        # Draw the background of the window
        pygame.draw.rect(self.screen, (50, 50, 50), self.rect)
        # Title
        font_title = pygame.font.Font(None, 24)
        title_surf = font_title.render(self.title, True, (255, 255, 255))
        self.screen.blit(title_surf, (self.rect.x + 10, self.rect.y + 10))
        # Content
        font_content = pygame.font.Font(None, 20)
        content_surf = font_content.render(self.content, True, (200, 200, 200))
        self.screen.blit(content_surf, (self.rect.x + 10, self.rect.y + 40))

    def handle_event(self, event):
        if event.type == pygame.MOUSEBUTTONDOWN:
            if self.rect.collidepoint(event.pos):
                self.visible = False  # Hide the window on any click within its bounds

class Button:
    def __init__(self, screen, rect, label):
        self.screen = screen
        self.rect = rect
        self.label = label
    
    def draw(self):
        pygame.draw.rect(self.screen, (100, 100, 100), self.rect)  # Draw the button
        font = pygame.font.Font(None, 20)
        text_surf = font.render(self.label, True, (255, 255, 255))
        text_rect = text_surf.get_rect(center=self.rect.center)
        self.screen.blit(text_surf, text_rect)
