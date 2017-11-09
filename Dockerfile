FROM python:2

WORKDIR /usr/src/app

RUN pip install --no-cache-dir -r requirements.txt

COPY /00-MachineLearn.py .

CMD [ "python", "./00-MachineLearn.py" ]
