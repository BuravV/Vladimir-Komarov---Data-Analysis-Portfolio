import telebot

from Report import make_report

#token
bot = telebot.TeleBot('BOT TOKEN')

@bot.message_handler(content_types=['text'])

def send_report(message):
    
    if message.text == "/send":  
        #creating report for today 
        make_report()     
        bot.send_message(message.from_user.id, "Report for today:")
        bot.send_photo(message.from_user.id,open('Data output/CCR cumulative.png','rb'))
        bot.send_photo(message.from_user.id,open('Data output/DUA.png','rb'))
        bot.send_photo(message.from_user.id,open('Data output/Daily Revenue.png','rb'))
        bot.send_message(message.from_user.id, "Have a nice day :)") 
    elif message.text == "/help":
        bot.send_message(message.from_user.id, "For Daily Report, write /send")  
    else:
        bot.send_message(message.from_user.id, "I don't understand you. For more info: /help")       
        
bot.polling(none_stop=True)