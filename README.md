##PAO YING CHUB
##game pao ying chub
play_game <- function(){
  print("Welcome to the game!")
  hands <- c("paper", "scissor", "rock")
  
  player_score <- 0
  bot_score <- 0
  round_results <- c()  # ใช้เก็บคะแนนแต่ละรอบ
  
  for(round in 1:10) {
    cat("\nRound", round, "\n")
    bot_hand <- sample(hands, 1)  # บอทสุ่มมือ
    
    # รับค่าของผู้เล่น
    player_hand <- readline("Please choose one option: rock, scissor, paper or type 'quit' to exit: ")
    
    # ตรวจสอบว่าผู้เล่นต้องการออกจากเกม
    if (player_hand == "quit") {
      cat("You quit the game. Final Score - Player:", player_score, "Bot:", bot_score, "\n")
      break
    }
    
    # ตรวจสอบค่าป้อนเข้า
    if (!(player_hand %in% hands)) {
      cat("Invalid choice! Please choose rock, scissor, or paper.\n")
      next  # ข้ามรอบนี้และให้เลือกใหม่
    }
    
    # แสดงผลที่เลือก
    cat("You chose:", player_hand, "\n")
    cat("Bot chose:", bot_hand, "\n")
    
    # ตัดสินผลการแข่งขัน
    if (player_hand == bot_hand) {
      cat("It's a tie!\n")
      round_results <- c(round_results, paste("Round", round, ": Tie"))
    } else if ((player_hand == "rock" & bot_hand == "scissor") |
               (player_hand == "scissor" & bot_hand == "paper") |
               (player_hand == "paper" & bot_hand == "rock")) {
      cat("You win this round!\n")
      player_score <- player_score + 1
      round_results <- c(round_results, paste("Round", round, ": Player wins"))
    } else {
      cat("Bot wins this round!\n")
      bot_score <- bot_score + 1
      round_results <- c(round_results, paste("Round", round, ": Bot wins"))
    }
  }
  
  # แสดงผลลัพธ์
  cat("\nGame Over! Final Score - Player:", player_score, "Bot:", bot_score, "\n\n")
  
  # แสดงสรุปผลแต่ละรอบ
  cat("Summary of Rounds:\n")
  cat(paste(round_results, collapse = "\n"), "\n")
  
  # ประกาศผู้ชนะ
  if (player_score > bot_score) {
    cat("\n🎉 Congratulations! You won the game! 🎉\n")
  } else if (player_score < bot_score) {
    cat("\n🤖 Bot wins the game! Better luck next time! 🤖\n")
  } else {
    cat("\n😲 It's a tie! What a close match! 😲\n")
  }
}


