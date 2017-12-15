import web
import time


urls = (
    '/', 'Seller',
    '/calc', 'Calculator'
)


render = web.template.render('templates', base='base')


class Item:

    def __init__(self, n):

        self.name = n


class Seller:

    def GET(self):

        items = [Item(1), Item(2), Item(3)]

        return render.products(items)


class Calculator:

    def GET(self):

        # inp = web.input()

        # numberOfItems = []

        # for it in inp.data.split(','):

        #     numberOfItems.append(it)

        # Generates data file and runs

        startTime = time.perf_counter()

        time.sleep(1)

        endTime = time.perf_counter()

        # Returns results

        frete = 0.0

        return render.frete(frete, "{0:5.2f}".format(endTime - startTime))


app = web.application(urls, globals())

if __name__ == '__main__':

    app.run()
