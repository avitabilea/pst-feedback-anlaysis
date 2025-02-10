import os
import pandas as pd
from openai import OpenAI
from dotenv import load_dotenv
import json
import time
from tqdm import tqdm
from datetime import datetime

env_path = 'C:/Users/yaj3ma/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/.env'
load_dotenv(env_path)

class FeedbackAnalyzer:
    def __init__(self, model="gpt-4o-mini"):
        print("Initializing FeedbackAnalyzer...")
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
        return f'''You are a researcher that analyzes feedback given to pre-service teachers after classroom observations.

        Text to analyze:
        {text}

        Key Analysis Rules:
        1. area_for_improvement should identify the teaching skill that is most prominently discussed as needing improvement in the feedback
        - Choose "none" if no areas are identified as needing improvement
        - Choose "multiple" if 2+ areas are equally emphasized as needing improvement
        - Choose "other" if the main area for improvement doesn't match the listed skills
        - Otherwise, choose the single skill most emphasized as needing improvement in the list of target_skills provided

        2. areas_mentioned should ONLY be true (1) when the feedback explicitly indicates that skill needs improvement
        - Set to false (0) if the skill is praised or mentioned positively
        - Set to false (0) if the skill is just mentioned descriptively without suggesting improvement
        - Set to true (1) ONLY if the feedback suggests this specific skill needs improvement

        Example interpretations:
        - "Your classroom management was excellent" → classroom_management: false
        - "Consider working on your classroom management" → classroom_management: true
        - "You took attendance and managed transitions" → classroom_management: false
        - "While your lesson planning was strong, your classroom management needs work" → 
            - area_for_improvement: "Classroom Management"
            - areas_mentioned.classroom_management: true
            - areas_mentioned.lesson_planning: false

        Analyze the text and respond with ONLY a JSON object (no other text) using the following structure:

        {{
            "area_for_improvement": (MUST be one of: {skills_list}, "other", "none", "multiple"),
            "areas_mentioned": {{
                "classroom_management": (boolean - true ONLY if feedback suggests improvement needed),
                "lesson_planning": (boolean - true ONLY if feedback suggests improvement needed),
                "differentiation": (boolean - true ONLY if feedback suggests improvement needed),
                "assessment_feedback": (boolean - true ONLY if feedback suggests improvement needed),
                "student_engagement": (boolean - true ONLY if feedback suggests improvement needed),
                "student_comprehension": (boolean - true ONLY if feedback suggests improvement needed),
                "communication": (boolean - true ONLY if feedback suggests improvement needed)
            }},
            "has_areas_for_improvement": (boolean indicating if any specific areas for improvement are mentioned),
            "has_praise": (boolean indicating if there is specific praise for the pre-service teacher),
            "has_lesson_retelling": (boolean indicating if the text mainly retells what happened in the lesson),
            "has_admin_notes": (boolean indicating if the text contains administrative or logistical notes),
            "evidence": {{
                "specific_examples": (boolean indicating if concrete examples from lesson are provided),
                "next_steps": (boolean indicating if practical and actionable next steps are suggested),
                "strengths_mentioned": (boolean indicating if specific strengths are highlighted),
                "areas_for_growth": (boolean indicating if specific growth areas are identified)
            }}
    }}
    
    CRITICAL RULE: area_for_improvement can ONLY be {skills_list}, "other", "none", or "multiple". This variable MUST NOT take on any other values.
    '''

    def analyze_feedback(self, text, max_retries=3):
        if not text or len(text.strip()) < 10:
            empty_areas = {skill.lower().replace(' ', '_'): False for skill in self.target_skills}
            return {
                "area_for_improvement": "none",
                "areas_mentioned": empty_areas,
                "has_areas_for_improvement": False,
                "has_praise": False,
                "has_lesson_retelling": False,
                "has_admin_notes": False,
                "evidence": {
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
                            "content": "You are an expert that analyzes feedback given to pre-service teachers after classroom observations. Respond ONLY with a valid JSON object."
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
                print(f"Error in feedback analysis (attempt {attempt+1}): {e}")
                time.sleep(1)

        return None

def process_feedback_analysis(input_file, output_file):
    print(f"\nStarting analysis at {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    
    try:
        print("Loading data...")
        df = pd.read_excel(input_file)
        print(f"Loaded {len(df)} rows of data")

        # Create intermediate directory if it doesn't exist
        intermediate_dir = os.path.join(os.path.dirname(output_file), 'intermediate')
        os.makedirs(intermediate_dir, exist_ok=True)

        analyzer = FeedbackAnalyzer()

        # Initialize columns with correct dtypes
        analysis_columns = {
            # String columns
            'area_for_improvement': 'none',
            
            # Binary indicators (0/1)
            'has_areas_for_improvement': 0,
            'has_praise': 0,
            'has_lesson_retelling': 0,
            'has_admin_notes': 0,
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
            'communication_mentioned': 0
        }

        # Create new DataFrame with observationid and text
        result_df = pd.DataFrame({
            'observationid': df['observationid'],
            'text': df['text_feedback']
        })

        # Initialize columns with correct dtypes
        for col, default_value in analysis_columns.items():
            if isinstance(default_value, str):
                result_df[col] = pd.Series(default_value, index=df.index, dtype='string')
            else:
                result_df[col] = pd.Series(default_value, index=df.index, dtype='int64')

        print("\nStarting feedback analysis...")
        for index, row in tqdm(df.iterrows(), total=len(df)):
            try:
                analysis_result = analyzer.analyze_feedback(row['text_feedback'])
                
                if analysis_result:
                    # Update area_for_improvement
                    result_df.at[index, 'area_for_improvement'] = analysis_result.get('area_for_improvement', 'none')
                    
                    # Update binary indicators
                    result_df.at[index, 'has_areas_for_improvement'] = int(analysis_result.get('has_areas_for_improvement', False))
                    result_df.at[index, 'has_praise'] = int(analysis_result.get('has_praise', False))
                    result_df.at[index, 'has_lesson_retelling'] = int(analysis_result.get('has_lesson_retelling', False))
                    result_df.at[index, 'has_admin_notes'] = int(analysis_result.get('has_admin_notes', False))

                    # Update evidence markers
                    evidence = analysis_result.get('evidence', {})
                    result_df.at[index, 'specific_examples'] = int(evidence.get('specific_examples', False))
                    result_df.at[index, 'next_steps'] = int(evidence.get('next_steps', False))
                    result_df.at[index, 'strengths_mentioned'] = int(evidence.get('strengths_mentioned', False))
                    result_df.at[index, 'areas_for_growth'] = int(evidence.get('areas_for_growth', False))

                    # Update area-specific indicators
                    areas_mentioned = analysis_result.get('areas_mentioned', {})
                    result_df.at[index, 'classroom_management_mentioned'] = int(areas_mentioned.get('classroom_management', False))
                    result_df.at[index, 'lesson_planning_mentioned'] = int(areas_mentioned.get('lesson_planning', False))
                    result_df.at[index, 'differentiation_mentioned'] = int(areas_mentioned.get('differentiation', False))
                    result_df.at[index, 'assessment_feedback_mentioned'] = int(areas_mentioned.get('assessment_feedback', False))
                    result_df.at[index, 'student_engagement_mentioned'] = int(areas_mentioned.get('student_engagement', False))
                    result_df.at[index, 'student_comprehension_mentioned'] = int(areas_mentioned.get('student_comprehension', False))
                    result_df.at[index, 'communication_mentioned'] = int(areas_mentioned.get('communication', False))

            except Exception as e:
                print(f"Error processing row {index}: {str(e)}")

            if index > 0 and index % 100 == 0:
                intermediate_file = os.path.join(intermediate_dir, f'intermediate_feedback_{index}.csv')
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
    input_file = r"C:/Users/yaj3ma/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/analysis/raw data/PST Data.xlsx"
    output_file = r"C:/Users/yaj3ma/Dropbox/Andrew and Brendan Shared Folder/PST Feedback Text/analysis/processed data/2025.02.10 - Feedback Analysis.xlsx"
    process_feedback_analysis(input_file, output_file)

if __name__ == "__main__":
    main()