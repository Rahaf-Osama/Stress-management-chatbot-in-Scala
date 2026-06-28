import scala.io.StdIn
import java.time.Instant
import scala.util.Random

//hla's code
//---------------------------------------------------------------------------------

case class Type(
  name: String,
  stressType: String,
  steps: List[String]
)

class StressChatbot {

  var history: List[String] = List()

  val types: List[Type] = List(
    Type(
      "Box Breathing",
      "panic",
      List(
        "Breathe in for 4 seconds",
        "Hold for 4 seconds",
        "Breathe out for 4 seconds",
        "Repeat 4 times"
      )
    ),
    Type(
      "4-7-8 Breathing",
      "anxiety",
      List(
        "Breathe in for 4 seconds",
        "Hold for 7 seconds",
        "Breathe out for 8 seconds",
        "Repeat slowly"
      )
    ),
    Type(
      "Study Break",
      "study",
      List(
        "Stop studying for 5 minutes",
        "Drink water",
        "Take deep breaths",
        "Start with one small task"
      )
    ),
    Type(
      "Sleep Routine",
      "sleep",
      List(
        "Put your phone away",
        "Dim the lights",
        "Take slow breaths",
        "Relax your body"
      )
    ),
    Type(
      "Write Your Thoughts",
      "overthinking",
      List(
        "Write what is stressing you",
        "Circle what you can control",
        "Choose one simple action"
      )
    ),
    Type(
      "Short Walk",
      "work",
      List(
        "Stand up",
        "Walk for 5 minutes",
        "Relax your shoulders",
        "Come back to one task"
      )
    )
  )

  def greetUser(): String = {
    "Hello! I am your Stress Management Chatbot.\n" +
      "I can help with anxiety, panic, study stress, work stress, sleep problems, and overthinking.\n" +
      "Type help to see examples, or type exit to stop."
  }

  def handleUserInput(input: String): String = {
    val words = parseInput(input)

    val response = generateResponse(words)

    history = history :+ input

    response
  }

  def parseInput(input: String): List[String] = {
    input
      .toLowerCase
      .replaceAll("[^a-z0-9\\s]", "")
      .split("\\s+")
      .toList
      .filter(word => word.nonEmpty)
  }

  def generateResponse(words: List[String]): String = {
  words match {
    case w if w.contains("hi") || w.contains("hello") || w.contains("hey") =>
      "Hi! Tell me how you feel today."

    case w if w.contains("happy") ||
      w.contains("good") ||
      w.contains("great") ||
      w.contains("love") ||
      w.contains("nice") ||
      w.contains("fun") ||
      w.contains("cool") ||
      w.contains("excited") ||
      w.contains("amazing") ||
      w.contains("awesome") ||
      w.contains("calm") ||
      w.contains("hopeful") ||
      w.contains("glad") ||
      w.contains("fine") ||
      w.contains("better") ||
      w.contains("enjoy") ||
      w.contains("perfect") ||
      w.contains("proud") ||
      w.contains("relaxed") ||
      w.contains("wonderful") ||
      w.contains("motivated") ||
      w.contains("okay") ||
      w.contains("smile") ||
      w.contains("fantastic") ||
      w.contains("excellent") =>
        "I'm glad to hear this. If you need help, contact me."

    case w if w.contains("help") =>
      val examples = List(
        "I feel anxious",
        "I have panic",
        "I have exam stress",
        "I cannot sleep",
        "I am overthinking",
        "recommend"
      )

      "You can say:\n" + examples.map(example => "- " + example).mkString("\n")

    case w if w.contains("panic") =>
      val t = types.find(x => x.stressType == "panic").get
      formatType(t)

    case w if w.contains("anxiety") || w.contains("anxious") || w.contains("worried") =>
      val t = types.find(x => x.stressType == "anxiety").get
      formatType(t)

    case w if w.contains("study") || w.contains("exam") =>
      val t = types.find(x => x.stressType == "study").get
      formatType(t)

    case w if w.contains("sleep") || w.contains("tired") =>
      val t = types.find(x => x.stressType == "sleep").get
      formatType(t)

    case w if w.contains("overthinking") || w.contains("thoughts") =>
      val t = types.find(x => x.stressType == "overthinking").get
      formatType(t)

    case w if w.contains("work") || w.contains("job") =>
      val t = types.find(x => x.stressType == "work").get
      formatType(t)

    case w if w.contains("recommend") || w.contains("suggest") =>
      val lowerHistory = history.map(sentence => sentence.toLowerCase)

      if (lowerHistory.exists(sentence => sentence.contains("panic"))) {
        val t = types.find(x => x.stressType == "panic").get
        "I recommend:\n" + formatType(t)
      } else if (lowerHistory.exists(sentence => sentence.contains("anxiety") || sentence.contains("anxious"))) {
        val t = types.find(x => x.stressType == "anxiety").get
        "I recommend:\n" + formatType(t)
      } else if (lowerHistory.exists(sentence => sentence.contains("study") || sentence.contains("exam"))) {
        val t = types.find(x => x.stressType == "study").get
        "I recommend:\n" + formatType(t)
      } else if (lowerHistory.exists(sentence => sentence.contains("sleep") || sentence.contains("tired"))) {
        val t = types.find(x => x.stressType == "sleep").get
        "I recommend:\n" + formatType(t)
      } else if (lowerHistory.exists(sentence => sentence.contains("overthinking") || sentence.contains("thoughts"))) {
        val t = types.find(x => x.stressType == "overthinking").get
        "I recommend:\n" + formatType(t)
      } else if (lowerHistory.exists(sentence => sentence.contains("work") || sentence.contains("job"))) {
        val t = types.find(x => x.stressType == "work").get
        "I recommend:\n" + formatType(t)
      } else {
        val t = types.find(x => x.stressType == "anxiety").get
        "I recommend starting with:\n" + formatType(t)
      }

    case w if w.contains("summary") =>
      "You talked to me " + history.length + " time(s)."

    case w if w.contains("thanks") || w.contains("thank") =>
      "You are welcome. Take care of yourself."

    case _ =>
      "I did not understand. Try saying: anxiety, panic, study stress, sleep, overthinking, or recommend."
  }
  }
  def formatType(t: Type): String = {
    t.name + "\n" + t.steps.map(step => "- " + step).mkString("\n")
  }
}

//yomna's code
//---------------------------------------------------------------------------------

case class Activity(
  name: String,
  category: String,
  duration: Int,
  mood: String
)


object RecommendationEngine {
  val Activities = List(
    Activity("deep breathing", "breathing", 5, "anxious"),
    Activity("meditation", "relaxation", 15, "stressed"),
    Activity("stretching", "exercise", 20, "tense"),
    Activity("short walk", "physical", 25, "tired"),
    Activity("journaling", "writing", 10, "overthinking"),
    Activity("tea break", "relaxation", 10, "tired"),
    Activity("reading", "mental", 15, "sad"),
    Activity("yoga", "exercise", 20, "stressed")
  )

  var userPreference: Map[String, String] = Map()

  def getUserPreference(): Map[String, String] = {
    userPreference
  }

  def updatePreference(
    key: String,
    value: String
  ): Map[String, String] = {
    val updatedPreferece = userPreference + (key -> value)
    userPreference = updatedPreferece
    updatedPreferece
  }

  def recommend(
    preferences: Map[String, String],
    data: List[Activity]
  ): List[Activity] = {

    preferences.get("mood") match {
      case Some(userMood) =>
        data.filter(activity => activity.mood == userMood).sortBy(activity => activity.duration)

      case None => Nil
    }
  }
  

  def explainRecommendation(
    item: Activity
  ): String = {
    "i recommend " + item.name + " because it helps when feeling " + item.mood + " and only takes " + item.duration + " minutes."
  }
}

//rahaf's code
//---------------------------------------------------------------------------------

case class InteractionEntry(
  sequenceNumber: Int,
  timestamp: Instant,
  userInput: String,
  botResponse: String,
  detectedIntent: String
)

case class ConversationState(
  history: List[InteractionEntry] = List.empty,
  userPreferences: Map[String, String] = Map.empty,
  currentTone: String = "Neutral"
)

def logInteraction(
  userInput: String,
  botResponse: String,
  context: ConversationState,
  detectedIntent: String
): ConversationState = {

  val newEntry = InteractionEntry(
    sequenceNumber = context.history.size + 1,
    timestamp = Instant.now(),
    userInput = userInput,
    botResponse = botResponse,
    detectedIntent = detectedIntent
  )

  context.copy(
    history = context.history :+ newEntry
  )
}

def getConversationHistory(
  context: ConversationState
): List[InteractionEntry] = {

  context.history
}

def getLastNInteractions(
  n: Int,
  context: ConversationState
): List[InteractionEntry] = {

  context.history.takeRight(n)
}

def detectRepeatedQuery(
  input: String,
  history: List[InteractionEntry]
): Boolean = {

  val normalizedInput =
    input.toLowerCase.trim

  history.exists { interaction =>

    val previousInput =
      interaction.userInput.toLowerCase.trim

    previousInput.contains(normalizedInput) ||
      normalizedInput.contains(previousInput)
  }
}

def detectIntent(input: String): String = {
  val lowerInput = input.toLowerCase

  if (lowerInput.contains("panic")) {
    "Panic"
  } else if (lowerInput.contains("anxiety") || lowerInput.contains("anxious") || lowerInput.contains("worried")) {
    "Anxiety"
  } else if (lowerInput.contains("study") || lowerInput.contains("exam")) {
    "StudyStress"
  } else if (lowerInput.contains("sleep") || lowerInput.contains("tired")) {
    "SleepProblem"
  } else if (lowerInput.contains("overthinking") || lowerInput.contains("thoughts")) {
    "Overthinking"
  } else if (lowerInput.contains("work") || lowerInput.contains("job")) {
    "WorkStress"
  } else if (lowerInput.contains("recommend") || lowerInput.contains("suggest")) {
    "Recommendation"
  } else if (lowerInput.contains("summary")) {
    "Summary"
  } else if (lowerInput.contains("help")) {
    "Help"
  } else {
    "GeneralInquiry"
  }
}

//hedaya's code
//---------------------------------------------------------------------------------

def extractTopics(history: List[InteractionEntry]): List[String] =
  history match {

    case Nil => Nil

    case head :: tail =>

      val inputs = head.userInput
        .toLowerCase
        .replaceAll("[^a-z0-9\\s]", "")
        .split("\\s+")
        .toList
        .filter(word => word.nonEmpty)

      def filterWords(list: List[String]): List[String] =
        list match {

          case Nil => Nil

          case h :: t if h.length > 3 =>
            h :: filterWords(t)

          case _ :: t =>
            filterWords(t)
        }

      val topics = filterWords(inputs)

      topics ++ extractTopics(tail)
  }
//------------------------------------------------------------------------------------------------------------------------------------

def getMostDiscussedTopics(history: List[InteractionEntry]): List[(String, Int)] = {

  val topics = history.map(_.detectedIntent)

  topics
  .groupBy(topic => topic)
  .map(pair => pair._1 -> pair._2.length)
  .toList
  .sortBy(pair => -pair._2)
  .take(5)
  //.sortBy((topic, count) => -count)
}
//------------------------------------------------------------------------------------------------------------------------------------------------

def summarizeConversation(history: List[InteractionEntry]): String = {

  val topics = getMostDiscussedTopics(history)

  topics match {

    case Nil =>
      "No topics were discussed yet."

    case first :: Nil =>
      "We've mainly discussed " + first._1 + "."

    case first :: second :: Nil =>
      "We've discussed " +
        first._1 +
        " and " +
        second._1 +
        "."

    case first :: second :: third :: tail =>
      "We've mostly discussed " +
        first._1 +
        ", " +
        second._1 +
        ", and " +
        third._1 +
        "."
  }
}
//--------------------------------------------------------------------------------------------------------------------------------

def getLastMessage(history: List[InteractionEntry]): String =
  history match {

    case head :: Nil =>
      head.userInput.toLowerCase

    case head :: tail =>
      getLastMessage(tail)

    case Nil =>
      ""
  }
//--------------------------------------------------------------------------------------------------------------------------------
def getUserMood(history: List[InteractionEntry]): String = {

 val positive_words = List(
  "happy", "good", "great", "love",
  "nice", "fun", "cool", "excited",
  "amazing", "awesome", "calm", "hopeful",
  "glad", "fine", "better", "enjoy",
  "perfect", "proud", "relaxed", "wonderful",
  "motivated", "okay", "smile", "fantastic",
  "excellent"
)
  val negative_words = List(
    "sad", "bad", "angry", "hate",
  "tired", "stress", "boring", "upset",
  "cry", "worried", "depressed", "annoyed",
  "hurt", "confused", "mad", "lonely",
  "afraid", "scared", "terrible", "awful",
  "pain", "sick", "nervous", "frustrated",
  "disappointed","mad"
  )

  val lastMessage =
    getLastMessage(history)

  if (positive_words.exists(lastMessage.contains))
    "Positive"

  else if (negative_words.exists(lastMessage.contains))
    "Negative"

  else
    "Neutral"
}
//bonus game mode
//---------------------------------------------------------------------------------

case class GameQuestion(
  situation: String,
  choices: List[String],
  correctAnswer: Int,
  explanation: String
)

def getGameQuestions(): List[GameQuestion] = {
  List(
    GameQuestion(
      "You have an exam tomorrow and you feel very stressed.",
      List(
        "Study for hours without any break",
        "Take a short break, drink water, then start with one small task",
        "Ignore studying completely"
      ),
      2,
      "Small breaks help your brain calm down, and starting with one small task makes studying feel easier."
    ),
    GameQuestion(
      "You suddenly feel panic and your breathing becomes fast.",
      List(
        "Try box breathing slowly",
        "Run away from everything",
        "Keep thinking about the panic"
      ),
      1,
      "Box breathing helps your body slow down and feel safer."
    ),
    GameQuestion(
      "You cannot sleep because you are using your phone in bed.",
      List(
        "Keep scrolling until you feel tired",
        "Put your phone away and dim the lights",
        "Drink a lot of coffee"
      ),
      2,
      "Putting the phone away and dimming the lights helps your body prepare for sleep."
    ),
    GameQuestion(
      "You are overthinking and many thoughts are in your head.",
      List(
        "Write your thoughts down and choose one thing you can control",
        "Keep all thoughts inside your head",
        "Blame yourself for thinking too much"
      ),
      1,
      "Writing thoughts down makes them clearer and helps you focus on what you can control."
    ),
    GameQuestion(
      "You feel tired and stressed from work.",
      List(
        "Sit in the same place and do nothing",
        "Take a short walk and relax your shoulders",
        "Open more tasks at the same time"
      ),
      2,
      "A short walk can refresh your body and mind, even if it is only for a few minutes."
    ),
    GameQuestion(
      "Your friend says they feel anxious before a presentation.",
      List(
        "Tell them to ignore their feelings",
        "Suggest slow breathing and positive self-talk",
        "Tell them the presentation will definitely go badly"
      ),
      2,
      "Supportive words and slow breathing can help someone feel calmer before speaking."
    ),
    GameQuestion(
      "You feel sad and do not want to do anything.",
      List(
        "Try a small relaxing activity like reading or drinking tea",
        "Stay alone and keep thinking negatively",
        "Tell yourself you are weak"
      ),
      1,
      "A small calm activity can help improve your mood step by step."
    )
  )
}

def askGameQuestion(question: GameQuestion, questionNumber: Int): Int = {
  println("\nQuestion " + questionNumber)
  println("--------------------------------")
  println(question.situation)

  println("\nChoose the best answer:")

  question.choices.zipWithIndex.foreach { pair =>
    val choice = pair._1
    val index = pair._2 + 1

    println(index + ". " + choice)
  }

  print("Your answer: ")
  val answer = StdIn.readLine().trim

  if (answer == question.correctAnswer.toString) {
    println("\nCorrect! Nice choice.")
    println(question.explanation)
    1
  } else {
    println("\nNot quite, but that's okay.")
    println("The best answer is: " + question.correctAnswer)
    println(question.explanation)
    0
  }
}

def showGameResult(score: Int, total: Int): Unit = {
  println("\nGame Finished")
  println("--------------------------------")
  println("Your score: " + score + " out of " + total)

  if (score == total) {
    println("Amazing! You really understand how to handle stress.")
  } else if (score >= total - 1) {
    println("Great job! You made very healthy choices.")
  } else if (score >= total / 2.0) {
    println("Good work! You know some helpful stress management ideas.")
  } else {
    println("Good try. The game is here to help you learn, not to judge you.")
  }
}

def playStressReliefGame(): Unit = {
  println("\nStress Relief Challenge")
  println("--------------------------------")
  println("You will see stressful situations.")
  println("Choose the healthiest response.")
  println("Try your best and learn from each answer.")

  val allQuestions = getGameQuestions()
  val selectedQuestions = Random.shuffle(allQuestions).take(5)

  var score = 0
  var questionNumber = 1

  selectedQuestions.foreach { question =>
    score = score + askGameQuestion(question, questionNumber)
    questionNumber = questionNumber + 1
  }

  showGameResult(score, selectedQuestions.length)
}

@main
def runProgram(): Unit = {
  val chatbot = new StressChatbot()
  var context = ConversationState()
  var running = true

  println("Stress Management System")
  println("------------------------")
  println(chatbot.greetUser())

  while (running) {
    println("\nChoose an option:")
    println("1. Chat with the stress chatbot")
    println("2. Get activity recommendation")
    println("3. View conversation summary")
    println("4. View last conversation")
    println("5. View most discussed topics")
    println("6. Extract topics from conversation")
    println("7. Detect user mood")
    println("8. Play stress relief game")
    println("9. Exit")
    print("Enter your choice: ")

    val choice = StdIn.readLine().trim

    choice match {

      case "1" =>
        print("\nYou: ")
        val input = StdIn.readLine()

        if (input.toLowerCase.trim == "exit") {
          println("Bot: Goodbye. Remember to breathe slowly.")
          running = false
        } else {
          val repeated = detectRepeatedQuery(input, context.history)

          val response = chatbot.handleUserInput(input)

          context = logInteraction(
            userInput = input,
            botResponse = response,
            context = context,
            detectedIntent = detectIntent(input)
          )

          if (repeated) {
            println("Bot: You asked something similar before.")
          }

          println("Bot: " + response)
        }

      case "2" =>
        print("\nEnter your mood: ")
        val mood = StdIn.readLine().toLowerCase.trim

        RecommendationEngine.updatePreference("mood", mood)

        val preferences = RecommendationEngine.getUserPreference()

        val recommendations = RecommendationEngine.recommend(
          preferences,
          RecommendationEngine.Activities
        )

        if (recommendations.isEmpty) {
          println("No recommendation found for this mood.")
          println("Try: anxious, stressed, tense, tired, overthinking, or sad.")
        } else {
          println("\nRecommendations:")

          recommendations.foreach { activity =>
            println("- " + RecommendationEngine.explainRecommendation(activity))
          }
        }

      case "3" =>
        println("\nConversation Summary")
        println("Total interactions: " + getConversationHistory(context).length)
        println(summarizeConversation(context.history))

      case "4" =>
        val lastInteractions = getLastNInteractions(1, context)

        if (lastInteractions.isEmpty) {
          println("\nNo conversation history yet.")
        } else {
          println("\nLast Conversation:")

          lastInteractions.foreach { interaction =>
            println("Number: " + interaction.sequenceNumber)
            println("Time: " + interaction.timestamp)
            println("User Input: " + interaction.userInput)
            println("Bot Response: " + interaction.botResponse)
            println("Detected Intent: " + interaction.detectedIntent)
          }
        }

      case "5" =>
        val topics = getMostDiscussedTopics(context.history)

        if (topics.isEmpty) {
          println("\nNo topics were discussed yet.")
        } else {
          println("\nMost Discussed Topics:")

          topics.foreach { topic =>
            println("- " + topic._1 + ": " + topic._2 + " time(s)")
          }
        }

      case "6" =>
        val topics = extractTopics(context.history)

        if (topics.isEmpty) {
          println("\nNo topics found yet.")
        } else {
          println("\nExtracted Topics:")

          topics.foreach { topic =>
            println("- " + topic)
          }
        }

      case "7" =>
        val mood = getUserMood(context.history)

        println("\nDetected User Mood: " + mood)

      case "8" =>
        playStressReliefGame()

      case "9" =>
        println("Goodbye.")
        running = false

      case _ =>
        println("Invalid choice. Please choose from 1 to 9.")
    }
  }
}