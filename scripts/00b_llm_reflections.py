import os
import pandas as pd
from openai import OpenAI
from dotenv import load_dotenv
import json
import time
from tqdm import tqdm
from datetime import datetime

env_path = 'C:/Users/Andre/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/.env'
load_dotenv(env_path)

class ReflectionAnalyzer:
    def __init__(self, model="gpt-4o-mini"):
        print("Initializing ReflectionAnalyzer...")
        self.client = OpenAI(
            api_key=os.getenv("OPENAI_API_KEY")
        )
        self.model = model
        self.target_skills = [
            "Classroom Management", 
            "Lesson Planning", 
            "Differentiation", 
            "Assessment and Feedback", 
            "Student Engagement", 
            "Student Comprehension", 
            "Communication"
        ]
        print("Initialization complete.")

    def generate_analysis_prompt(self, text):
        skills_list = ", ".join(self.target_skills)
        valid_values = skills_list + ', "other", "none", "multiple"'
        
        return f'''You are a researcher that analyzes reflections written by pre-service teachers (PSTs) after classroom observations.

        Text to analyze:
        {text}

        Analysis Rules for quality indicators:
        1. quality_indicators should identify whether the reflection contains specific measures that are related to reflection quality (i.e., specific examples, next steps, strengths, and areas for growth)
        - quality_indicators.specific_examples should flag whether the reflection contains specific examples of things that occurred during the lesson
        - quality_indicators.next_steps should flag whether the reflection contains clear, actionable next steps for the PST
        - quality_indicators.strengths should flag whether the reflection contains comments on areas or things that the PST did well
        - quality_indicators.areas_for_growth should flag whether the reflection contains specific growth areas for the PST to improve upon.
        - CRITICALLY IMPORTANT: areas_for_growth is a function of area_for_improvement and should only be 1 if area_for_improvement != "none"

        Examples for quality indicators:
        - "One thing that I think I do well is giving examples that the students can relate to in order to keep them engaged. I think that I have good discipline and my students 
           really respect me because of it. I think that I also had good energy. It is so much easier to keep students engaged when you are excited about what you are teaching. 
           I need to get better about blatantly stating the objective. I can seem to use the right phrasing."
            - quality_indicators.specific_examples: false
            - quality_indicators.next_steps: false
            - quality_indicators.strengths: true
            - quality_indicators.areas_for_growth: true

        - "This day wasn't as great of a "lesson" exactly. They way timing worked out with schedules and 9 - week exams just forced us in to a corner on this one. 
           Nevertheless, I still found a way to bring in some teaching points. The students did test corrections, and taught me how to analyze the data to go over the most missed questions, 
           and evaluate what may have caused the mistakes. I took those most missed questions and showed the kids what they did wrong with the use of the document camera and the smart board. 
           It was somewhat dry at times, but I noticed I got a lot of faces lighting up and noises when we went back through the questions together. 
           It was nice to see my students learning from their mistakes, and because of this process their grades improve on every test for the most part."
            - quality_indicators.specific_examples: true
            - quality_indicators.next_steps: false
            - quality_indicators.strengths: true
            - quality_indicators.areas_for_growth: false

        Analysis Rules for Area for Improvement:
        1. area_for_improvement should identify the teaching skill that is most prominently discussed as needing improvement in the reflection
        - Choose "none" if no areas are identified as needing improvement
        - Choose "multiple" if 2+ areas are equally emphasized as needing improvement
        - Choose "other" if the main area for improvement doesn't match the listed skills
        - Otherwise, choose the single skill most emphasized as needing improvement in the list of target_skills provided
        - CRITICALLY IMPORTANT area_for_improvement should be a function of areas_mentioned. For example, if there are multiple areas_mentioned flagged as true, this should be "multiple"
        - CRITICALLY IMPORTANT area_for_improvement can ONLY be {skills_list}, "other", "none", or "multiple". This variable MUST NOT take on any other values.

        2. areas_mentioned should ONLY be true (1) when the reflection explicitly indicates that a skill needs improvement
        - Set to false (0) if the skill is praised or mentioned positively
        - Set to false (0) if the skill is just mentioned descriptively without suggesting improvement
        - Set to true (1) ONLY if the reflection suggests this specific skill needs improvement

        Example interpretations:
        - "My classroom management (using hand signals, certain phrases) is getting better with time. The math subject has always made me nervous to teach, but as I practice teaching 
           it more I am getting more confident in teaching math and math strategies. What I would have done differently (and what I need to improve on) is time management and keeping station time / 
           teacher table around 15 - 20 minutes. My goal is to keep better track of students' activities in stations while I am at the teacher table (and redirecting off - task behavior in these stations)."
            - area_for_improvement: "Classroom Management"
            - areas_mentioned.classroom_management: true
            - areas_mentioned.lesson_planning: false
        - "My classroom management was excellent" → classroom_management: false
        - "I need to work on my classroom management" → classroom_management: true
        - "I took attendance and managed transitions" → classroom_management: false
        - "While my lesson planning was strong, my classroom management needs work" → 
            - area_for_improvement: "Classroom Management"
            - areas_mentioned.classroom_management: true
            - areas_mentioned.lesson_planning: false

        Analyze the text and respond with ONLY a JSON object (no other text) using the following structure:

        {{
            "area_for_improvement": (MUST be one of: {skills_list}, "other", "none", "multiple"),
            "areas_mentioned": {{
                "classroom_management": (boolean - true ONLY if feedback suggests improvement in classroom management is needed),
                "lesson_planning": (boolean - true ONLY if feedback suggests improvement in lesson planning is needed),
                "differentiation": (boolean - true ONLY if feedback suggests improvement in differentiation is needed),
                "assessment_feedback": (boolean - true ONLY if feedback suggests improvement in assesement/feedback is needed),
                "student_engagement": (boolean - true ONLY if feedback suggests improvement in student engagement is  needed),
                "student_comprehension": (boolean - true ONLY if feedback suggests improvement in student comprehension is needed),
                "communication": (boolean - true ONLY if feedback suggests improvement in communication is needed),
                "other": (boolean - true ONLY if feedback suggests improvement in a skill that is not in {skills_list} is needed)
            }},
            "quality_indicators": {{
                "specific_examples": (boolean indicating if concrete examples from lesson are provided),
                "next_steps": (boolean indicating if practical and actionable next steps are suggested),
                "strengths_mentioned": (boolean indicating if specific strengths are highlighted),
                "areas_for_growth": (boolean indicating if specific growth areas are identified)
            }}
        }}
        '''

    def analyze_reflections(self, text, max_retries=3):
        if not text or len(text.strip()) < 10:
            empty_areas = {skill.lower().replace(' ', '_'): False for skill in self.target_skills}
            return {
                "area_for_improvement": "none",
                "areas_mentioned": empty_areas,
                "quality_indicators": {
                    "specific_examples": False,
                    "next_steps": False,
                    "strengths_mentioned": False,
                    "areas_for_growth": False
                }
            }

        for attempt in range(max_retries):
            try:
                response = self.client.chat.completions.create(
                    model=self.model,
                    messages=[
                        {
                            "role": "system",
                            "content": "You are an expert that analyzes reflections written by pre-service teachers after classroom observations. Respond ONLY with a valid JSON object."
                        }, 
                        {
                            "role": "user",
                            "content": self.generate_analysis_prompt(text)
                        }
                    ],
                    temperature=0.2,
                    seed=42
                )

                try:
                    content = response.choices[0].message.content.strip()
                    content = content.replace('```json', '').replace('```', '').strip()
                    analysis_result = json.loads(content)
                    return analysis_result

                except json.JSONDecodeError as e:
                    print(f"JSON parsing error. Raw response: {content}")
                    print(f"Error details: {e}")

            except Exception as e:
                print(f"Error in reflection analysis (attempt {attempt+1}): {e}")
                time.sleep(1)

        return None

def process_reflection_analysis(input_file, output_file):
    print(f"\nStarting analysis at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    try:
        print("Loading data...")
        df = pd.read_excel(input_file)
        print(f"Loaded {len(df)} rows of data")

        # Create intermediate directory if it doesn't exist
        intermediate_dir = os.path.join(os.path.dirname(output_file), 'intermediate')
        os.makedirs(intermediate_dir, exist_ok=True)

        analyzer = ReflectionAnalyzer()

        # Initialize columns with correct dtypes
        analysis_columns = {
            # String columns
            'area_for_improvement': 'none',
            
            # Binary indicators (0/1)
            'specific_examples': 0,
            'next_steps': 0,
            'strengths_mentioned': 0,
            'areas_for_growth': 0,
            
            # Area-specific indicators
            'classroom_management_mentioned': 0,
            'lesson_planning_mentioned': 0,
            'differentiation_mentioned': 0,
            'assessment_feedback_mentioned': 0,
            'student_engagement_mentioned': 0,
            'student_comprehension_mentioned': 0,
            'communication_mentioned': 0,
            'other_mentioned': 0
        }

        # Create new DataFrame with observationid and text
        result_df = pd.DataFrame({
            'observationid': df['observationid'],
            'text': df['text_reflection']
        })

        # Initialize columns with correct dtypes
        for col, default_value in analysis_columns.items():
            if isinstance(default_value, str):
                result_df[col] = pd.Series(default_value, index=df.index, dtype='string')
            else:
                result_df[col] = pd.Series(default_value, index=df.index, dtype='int64')

        print("\nStarting reflection analysis...")
        for index, row in tqdm(df.iterrows(), total=len(df)):
            try:
                analysis_result = analyzer.analyze_reflections(row['text_reflection'])
                
                if analysis_result:
                    # Update area_for_improvement
                    result_df.at[index, 'area_for_improvement'] = analysis_result.get('area_for_improvement', 'none')

                    # Update quality markers
                    quality_indicators = analysis_result.get('quality_indicators', {})
                    result_df.at[index, 'specific_examples'] = int(quality_indicators.get('specific_examples', False))
                    result_df.at[index, 'next_steps'] = int(quality_indicators.get('next_steps', False))
                    result_df.at[index, 'strengths_mentioned'] = int(quality_indicators.get('strengths_mentioned', False))
                    result_df.at[index, 'areas_for_growth'] = int(quality_indicators.get('areas_for_growth', False))

                    # Update area-specific indicators
                    areas_mentioned = analysis_result.get('areas_mentioned', {})
                    result_df.at[index, 'classroom_management_mentioned'] = int(areas_mentioned.get('classroom_management', False))
                    result_df.at[index, 'lesson_planning_mentioned'] = int(areas_mentioned.get('lesson_planning', False))
                    result_df.at[index, 'differentiation_mentioned'] = int(areas_mentioned.get('differentiation', False))
                    result_df.at[index, 'assessment_feedback_mentioned'] = int(areas_mentioned.get('assessment_feedback', False))
                    result_df.at[index, 'student_engagement_mentioned'] = int(areas_mentioned.get('student_engagement', False))
                    result_df.at[index, 'student_comprehension_mentioned'] = int(areas_mentioned.get('student_comprehension', False))
                    result_df.at[index, 'communication_mentioned'] = int(areas_mentioned.get('communication', False))
                    result_df.at[index, 'other_mentioned'] = int(areas_mentioned.get('other', False))

            except Exception as e:
                print(f"Error processing row {index}: {str(e)}")

            if index > 0 and index % 100 == 0:
                intermediate_file = os.path.join(intermediate_dir, f'intermediate_reflection_{index}.csv')
                result_df.to_csv(intermediate_file, index=False)
                print(f"\nSaved intermediate results to {intermediate_file}")

        print("\nSaving final results...")
        result_df.to_csv(output_file, index=False)
        print(f"Analysis complete. Results saved to {output_file}")

    except Exception as e:
        print(f"Critical error in process_feedback_analysis: {str(e)}")
        raise

def main():
    load_dotenv()  
    input_file = r"C:/Users/Andre/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/analysis/raw data/PST Data.xlsx"
    output_file = r"C:/Users/Andre/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/analysis/processed data/2025.04.30 - Reflections Analysis.csv"
    process_reflection_analysis(input_file, output_file)

if __name__ == "__main__":
    main()