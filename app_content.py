import pygame
from monoboxPygame import Textbox, Label
import cv2
import numpy as np

class AppContent:
    def __init__(self, screen, rect, prolog, app_manager):
        self.screen = screen
        self.rect = rect
        self.prolog = prolog

    def handle_event(self, event):
        raise NotImplementedError

    def draw(self):
        raise NotImplementedError

class NotesApp(AppContent):
    def __init__(self, screen, rect, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        self.app_manager = app_manager
        # Initialize the textbox with margins
        self.initialize_textbox(rect)

    def initialize_textbox(self, rect):
        # Create a padded rectangle inside the provided rect
        padded_rect = pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 20, rect.height - 40)
        self.textbox = Textbox(rect=padded_rect, size=12)
        self.textbox.add('Notes: Prolog Syntax for Deduction Engine\n------------------\nevidence(suspect, type of evidence, details).\n\nlab_access(suspect, true/false).\n\nbackground(suspect, info).\n\nOnce all the necessary evidence is gathered, the Deduction Engine will compute whether the suspect was indeed the criminal.')

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

class TerminalApp(AppContent):
    def __init__(self, screen, rect, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        # Initialize the textbox with margins
        self.app_manager = app_manager
        self.rect = rect
        self.prolog = prolog
        self.command_input = Textbox(rect=pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 20, 30), size=11)
        self.output_display = Label(rect=pygame.Rect(rect.x + 10, rect.y + 70, rect.width - 20, rect.height - 100), size=9)
        self.command_input.add('>>help()')
        self.video_player = None

    def update_position(self, new_rect):
        # Update the main rect and then adjust for internal padding
        self.rect = new_rect
        self.command_input.move(new_rect.x + 10, new_rect.y + 30)
        self.command_input.resize(new_rect.width - 20, 30)
        self.output_display.move(new_rect.x + 10, new_rect.y + 70)
        self.output_display.resize(new_rect.width - 20, new_rect.height - 100)

        if self.video_player and self.video_player.visible:
            self.video_player.update_position(new_rect)

    def handle_event(self, event):
        if self.video_player and self.video_player.visible:
            self.video_player.handle_event(event)  # Let the video player handle events if it's active
        else:
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_RETURN:
                    command = self.command_input.toString().strip()
                    if command.startswith('>>'):
                        command = command[2:]
                    if command == 'help()':
                        command = 'help(all)'
                    self.run_command(command)
                    self.command_input.empty()
                    self.command_input.add(u'>>')
                else:
                    
                    self.command_input.update([event])

    def run_command(self, command):
        try:
            func, arg = command.split('(')
            arg = arg.strip(')')
            query = f"handle_command({func}('{arg}'), Result)"
            solutions = list(self.prolog.query(query))
            if solutions:
                result = solutions[0]['Result']
                if func == 'security_log':
                    self.launch_video_player(result)  # Launch the video player if the command is security_log
                else:
                    self.output_display.set(str(result))
            else:
                self.output_display.set("No result or invalid command.")
        except Exception as e:
            self.output_display.set(f"Error: {str(e)}")

    def launch_video_player(self, video_path):
        # Here we launch the video player window
        if self.video_player:
            self.video_player.close()  # Close existing video player if open
        self.video_player = VideoPlayerApp(self.screen, self.rect, video_path, self.prolog, self.app_manager)
        self.video_player.visible = True  # Make the video player visible

    def draw(self):
        if not self.video_player or not self.video_player.visible:
            self.command_input.render()
            self.output_display.render()
        if self.video_player and self.video_player.visible:
            self.video_player.draw()

class VideoPlayerApp(AppContent):
    def __init__(self, screen, rect, video_path, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        self.video_path = video_path
        self.cap = cv2.VideoCapture(self.video_path)
        self.video_fps = self.cap.get(cv2.CAP_PROP_FPS)
        self.frame = None
        self.playing = True
        self.close_button = Button(screen, rect=pygame.Rect(rect.left + 5, rect.y + 15, 15, 15), label='<')
        self.load_frame()  # Load the first frame

    def load_frame(self):
        ret, frame = self.cap.read()
        if not ret:
            self.cap.set(cv2.CAP_PROP_POS_FRAMES, 0)  # Rewind if we're at the end of the video
            ret, frame = self.cap.read()
        # Resize frame to fit app window dimensions
        frame = cv2.resize(frame, (self.rect.width - 10, self.rect.height - 30), interpolation=cv2.INTER_AREA)
        frame = cv2.cvtColor(frame, cv2.COLOR_BGR2RGB)  # Convert to RGB
        frame = np.rot90(frame)
        frame = np.flipud(frame)
        self.frame = pygame.surfarray.make_surface(frame)

    def handle_event(self, event):
        if self.playing:
            if event.type == pygame.MOUSEBUTTONDOWN and self.close_button.rect.collidepoint(event.pos):
                self.close()

    def update_position(self, new_rect):
        self.rect = new_rect  # Update the video display area
        self.close_button.rect = pygame.Rect(new_rect.left + 5, new_rect.y + + 15, 15, 15)
        self.load_frame()  # Reload the frame to fit new dimensions

    def draw(self):
        if not self.visible:
            return
        if self.playing:
            self.close_button.draw()
            self.load_frame()
        if self.frame:
            self.screen.blit(self.frame, (self.rect.x + 5, self.rect.y + 30))

    def close(self):
        self.cap.release()
        self.visible = False  # Hide the window
        self.playing = False  # Stop video playback

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
    def __init__(self, screen, rect, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        self.notifications = []
        self.app_manager = app_manager
        self.sent_messages = set()
        self.add_message("Partner", "Dr. Vostok went missing! Look into Vostok's last email.")

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
        if message not in self.sent_messages:
            new_rect = pygame.Rect(self.rect.x + 10, self.rect.y + 20 + len(self.notifications) * 35, self.rect.width - 20, 30)
            self.notifications.append(Notification(sender, message, self.screen, new_rect))
            self.sent_messages.add(message)
            # Automatically open the app window to show the new message
            com_link_app = self.app_manager.get_app('ComLink')
            if com_link_app:
                self.app_manager.open_app(com_link_app)
        
        self.update_display()

    def update_display(self):
        # Redraw notifications
        for notification in self.notifications:
            notification.draw()

class DeductionEngineApp(AppContent):
    def __init__(self, screen, rect, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        self.prolog = prolog
        self.app_manager = app_manager
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
        clues_text = self.textbox.toString()  
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

    def check_for_messages(self, evidence_type):
        messages = {
            "ethical_violation": "We need more evidence! Check access logs from vostok_lab." ,
            "bribe": "We are so close! We need evidence of the act itself!",
            "footage": "We did it! Congrats boss! I will send everything to the police asap."
        }
        com_link_app = self.app_manager.get_app('ComLink')
        if evidence_type in messages:
            com_link_app.content.add_message("Partner: ", messages[evidence_type])

        

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

            
            formatted_clue = clue.replace('evidence(', '').rstrip(')')
            parts = formatted_clue.split(',')
            print("Formatted parts:", parts)
            evidence_type = f'{parts[1].strip()}'
            

            # Check for messages
            self.check_for_messages(evidence_type)

    def print_knowledge_base(self):
        try:
            print("Current state of the knowledge base:")
            for result in self.prolog.query("list_facts"):
                print(result)
        except Exception as e:
            print(f"Error printing knowledge base: {e}")


class DataSleuthApp(AppContent):
    def __init__(self, screen, rect, prolog, app_manager):
        super().__init__(screen, rect, prolog, app_manager)
        self.text_input = Textbox(rect=pygame.Rect(rect.x + 10, rect.y + 30, rect.width - 120, 20), size=14)
        self.search_button = Button(screen, rect=pygame.Rect(rect.right - 110, rect.y + 30, 100, 25), label="Search")
        self.results = []  # This will hold SearchResult instances
        self.showing_details = False
        self.content_start_y = 0
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
                            
                            self.current_person = result.get_details()
                            self.detail_title = result.title
                            self.detail_content = result.content
                            self.showing_details = True


    def draw(self):
        if self.showing_details:
            if 'Details' in self.current_person:
                font = pygame.font.Font(None, 18)
                title_surf = font.render(self.detail_title, True, (255, 255, 255))
                self.screen.blit(title_surf, (self.rect.x + 10, self.rect.y + 50))

                self.content_start_y = self.rect.y + 70  
                font = pygame.font.Font(None, 16)
                

                # Call wrap_text to get lines
                lines = self.wrap_text(self.detail_content, font, self.rect.width - 20)

                for line in lines:
                    content_surf = font.render(line, True, (255, 255, 255))
                    self.screen.blit(content_surf, (self.rect.x + 10, self.content_start_y))
                    self.content_start_y += 20  # Increment y position for next line

                if "Alert" in self.current_person:
                    content_surf = font.render(self.current_person['Alert'], True, (255, 0, 0))
                    self.screen.blit(content_surf, (self.rect.x + 10, self.content_start_y))
                    self.content_start_y += 20

                self.close_button.draw()
            else:
                self.draw_detail_view()
           
        else:
            self.text_input.render()
            self.search_button.draw()
            y_offset = 60  # Starting y offset for results
            for result in self.results:
                result.rect.y = self.rect.y + y_offset
                result.draw()
                y_offset += 40   # Adjust y offset for the next result

    def wrap_text(self, text, font, max_width):
        words = text.split(' ')
        lines = []
        while words:
            line = ''
            while words and font.size(line + words[0])[0] <= max_width:
                line += (words.pop(0) + ' ')
            lines.append(line)
        return lines
    def draw_detail_view(self):
        self.close_button.draw()
        font = pygame.font.Font(None, 16)
        title_surf = font.render(self.detail_title, True, (255, 255, 255))
        self.screen.blit(title_surf, (self.rect.x + 10, self.rect.y + 40))

        # Picture on the left
        picture_surf = pygame.image.load(self.current_person['Picture']).convert()  # Load and convert the picture
        
        # Resize the picture
        picture_surf = pygame.transform.scale(picture_surf, (50, 70))
            
        picture_rect = picture_surf.get_rect(topright=(self.rect.x + 60, self.rect.y + 60))
        self.screen.blit(picture_surf, picture_rect)

        # Details on the right
        details = [
            f"DOB: {self.current_person['Date of Birth']}",
            f"City: {self.current_person['City of Birth']}",
            f"Email: {self.current_person['Email']}",
            f"Previous Employer: {self.current_person['Previous Employer']}"
        ]

        
        # Position for the first line of detailed text
        detail_text_y = self.rect.y + 60
        
        for detail in details:
            content_surf = font.render(detail, True, (200, 200, 200))
            self.screen.blit(content_surf, (picture_rect.right + 10, detail_text_y))
            detail_text_y += 20  

        if "Alert" in self.current_person:
            # Call wrap_text to get lines
            lines = self.wrap_text(self.current_person['Alert'], font, self.rect.width - 20)

            for line in lines:
                content_surf = font.render(line, True, (255, 0, 0))
                self.screen.blit(content_surf, (picture_rect.right + 10, detail_text_y))
                detail_text_y += 20  # Increment y position for next line


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
                    parts = result_data[result_data.find("(")+1:result_data.rfind(")")].split(", ", 2)
                    name = parts[0]
                    affiliation = parts[1]
                    self.results.append(SearchResult(self.prolog, result_data, name, f"Works at {affiliation}", None, self.screen, pygame.Rect(self.rect.x + 10, self.rect.y + 10, self.rect.width - 20, 30)))
                elif "article(" in result_data:
                    # Extract data for an article
                    parts = result_data[result_data.find("(")+1:result_data.rfind(")")].split(", ", 2)
                    title = parts[0]
                    content = parts[1]
                    alert = parts[2]
                    self.results.append(SearchResult(self.prolog, result_data, title, content, alert, self.screen, pygame.Rect(self.rect.x + 10, self.rect.y + 10, self.rect.width - 20, 30)))
        finally:
            query_result.close()

        print(f"Search results for '{query}': {len(self.results)} found")
        for res in self.results:
            print("Added Result:", res.title)




class SearchResult:
    def __init__(self, prolog, result, title, content, alert, screen, rect):
        self.alert = alert or None
        self.prolog = prolog
        self.result = result
        self.title = title
        self.content = content 
        self.formmated_content = self.format_content(content)
        self.screen = screen
        self.rect = rect

    def draw(self):
        pygame.draw.rect(self.screen, (100, 100, 100), self.rect) 
        font = pygame.font.Font(None, 20)
        title_surf = font.render(self.title, True, (255, 255, 255))
        content_surf = font.render(self.formmated_content, True, (200, 200, 200))
        self.screen.blit(title_surf, (self.rect.x + 5, self.rect.y + 1))
        self.screen.blit(content_surf, (self.rect.x + 5, self.rect.y + 16))

    def format_content(self, content):
        """ Truncate content to 30 characters, appending '...' if longer. """
        if len(content) > 30:
            return content[:28] + '...'
        return content
    
    def get_details(self):
        # Query Prolog for the detailed information about this person
        details_query = f"person('{self.title}', Affiliation, _, DateOfBirth, CityOfBirth, Email, PreviousEmployer, Picture, Alert)"
        details = next(self.prolog.query(details_query), None)
        
        if details:
            # Extract the details from the Prolog query result
            dob = details['DateOfBirth']
            affiliation = details['Affiliation']
            city = details['CityOfBirth']
            email = details['Email']
            prev_emp = details['PreviousEmployer']
            picture = details['Picture']
            alert = details['Alert']

            data = {
                "Name": self.title,
                "Affiliation": affiliation,
                "Date of Birth": dob,
                "City of Birth": city,
                "Email": email,
                "Previous Employer": prev_emp,
                "Picture": picture
            }
            if alert:
                data = {
                    "Alert": alert,
                    "Name": self.title,
                    "Affiliation": affiliation,
                    "Date of Birth": dob,
                    "City of Birth": city,
                    "Email": email,
                    "Previous Employer": prev_emp,
                    "Picture": picture
                    }
                return data
            return data
        else:
            # Return basic information if Prolog query fails or no details are found
            data = {
                "Name": self.title,
                "Affiliation": self.content,
                "Details": "No additional information available.",
                "Alert": self.alert
            }
            
            return data


class Button:
    def __init__(self, screen, rect, label):
        self.screen = screen
        self.rect = rect
        self.label = label
    
    def draw(self):
        pygame.draw.rect(self.screen, (100, 100, 100), self.rect)  
        font = pygame.font.Font(None, 20)
        text_surf = font.render(self.label, True, (255, 255, 255))
        text_rect = text_surf.get_rect(center=self.rect.center)
        self.screen.blit(text_surf, text_rect)
