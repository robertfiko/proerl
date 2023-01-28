FROM swipl

COPY . /app

#CMD ["ls", "app/test"]
WORKDIR /app
CMD ["sh", "test/test.sh"]